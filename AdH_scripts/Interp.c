/* This Program reads a file Initi.txt to get interpolation radius */
/* File Grid.3dm to get the ADH grid */
/* Scatter.txt to get the Scatter file in Space delimited XYZ format, first line of the file has to be X Y Z */
/* Performs Inverse Distance Squared Interpolation and write out gridxyx.txt */
/* GSAVANT DEC 15 2011 */
/* After the MAIN program are cannibilized ADH routines to read files etc... */
/* USAGE Inter.exe gridname.3dm scatter_file_name interpolation_radius */
/* eg if the scatter name is scatter.txt, the grid is goofy.3dm  and 15 m/ft/miles is the interp radius*/
/* The syntax is Interp.exe goofy.3dm scatter.txt 15 */
/* The interp distance is the same units as your grid */


#include "stdio.h"
#include "stdlib.h"
#include "math.h"

#define MAXLINE 1000
#define MAXCARD 3
#define NDPRELM 4
#define CARD_ND  19314
#define CARD_E3T  7975
#define UNSET_INT -3 /* an integer that has not been set */
#define SMALL8 1.0e-8

#define MAX_NNX 1000
#define MAX_NNY 1000
#define FACTOR 50

/* STRUCTURES */
typedef struct
{
  double x, y, z;
  int type, mat;
  int nodes[3];
  int flag;
} VECT;

/* Prototypes */
int init_split(
    char *,
    char *
);

int init_read_idatum(
    char *,
    char *
);

double init_read_ddatum(
    char *,
    char *
);


typedef struct nodes_entry
{
  struct nodes_entry *next;
  int node_number;
} NODES_ENTRY;


/* Macros */
#define MIN(a,b) (a < b ? a : b)
#define MAX(a,b) (a > b ? a : b)
  
/*
 * Interpolate Scatter point bathymetry data onto ADH nodes 
 *
 * usage:
 *        interp_scatter_to_adh 3dm_filename scatter_filename interp_distance
 *
 */


int main(int argc, char *argv[])
{
  FILE *fp_3dm;
  FILE *fp_scatter;
  FILE *fp_out;
  char line[MAXLINE];
  char data[MAXLINE];
  char name1[MAXLINE];

  int nnode = 0;
  int nelem2d = 0;
  int number;
  VECT *node;
  VECT *scatter_node;
  VECT *elem2d;

  double xx, yy, zz;
  double interp_dist;
  double x_max = -9.9e-99;
  double y_max = -9.9e-99;
  double x_min =  9.9e+99;
  double y_min =  9.9e+99;
  int nnx, nny;
  double xL, yL;
  int inode;
  int index;
  int index_x, index_y;
  int ii, jj;
  int numscat;
  int ix_start, ix_end, iy_start, iy_end;
  int imat;
  int nd0, nd1, nd2;
  double distance;
  double denom, numer;
  int found;
  int inter;
  int num_scatter;

  NODES_ENTRY ***nodes_hash;
  NODES_ENTRY *entry;

  /* Check arguments */
  if (argc < 4)
    {
      printf(" Incorrect usage:  interp_adh 3dm_filename scatter_filename interp_distance\n");
      exit(0);
    }

  fp_3dm = fopen(argv[1], "r");
  fp_scatter = fopen(argv[2], "r");
  interp_dist = strtod(argv[3], NULL);
  inter = interp_dist;
  sprintf(name1, "grid%d.3dm", inter);
  fp_out = fopen(name1, "w");

  /* READ THE 3DM FILE FOR THE ADH GRID. EXTRACT BOUNDING BOX */

  /* First, count the number of nodes */
  while(fgets(line, MAXLINE, fp_3dm) != NULL) 
    {
	/* splits the line and goes to the appropriate case */ 
	switch (init_split(line, data))
	{
        case CARD_E3T:
         nelem2d++;
        break;
	case CARD_ND:
          nnode++;
          break;
        default:
          break;
	}
    }
  rewind(fp_3dm);

  node = (VECT *) malloc(sizeof(VECT) * nnode);
  elem2d = (VECT *) malloc(sizeof(VECT) * nelem2d);
  while(fgets(line, MAXLINE, fp_3dm) != NULL)
    {
      switch (init_split(line, data))
        {
        case CARD_E3T:
         	    /* reads the data */ 
	  number = init_read_idatum(line, data);
	  nd0 = init_read_idatum(line, data);
	  nd1 = init_read_idatum(line, data);
	  nd2 = init_read_idatum(line, data);
	  imat = init_read_idatum(line, data);
	  
	  elem2d[number - 1].nodes[0] = nd0;
	  elem2d[number - 1].nodes[1] = nd1;
	  elem2d[number - 1].nodes[2] = nd2;
          elem2d[number - 1].mat = imat;
        break;
	case CARD_ND:
          /* reads the data */ 
	  number = init_read_idatum(line, data);
	  xx = init_read_ddatum(line, data);
	  yy = init_read_ddatum(line, data);
	  zz = init_read_ddatum(line, data);

          node[number - 1].x = xx;
          node[number - 1].y = yy;
          node[number - 1].z = zz;

          x_min = MIN(xx, x_min);
          x_max = MAX(xx, x_max);
          y_min = MIN(yy, y_min);
          y_max = MAX(yy, y_max);
          break;
        default:
          break;
	}
    }

  /* SETUP THE HASH FOR NODES */

  /* increase the bounding box to allow for scatter points that lie outside
   * the ADH grid
   */
  x_min -= interp_dist;
  y_min -= interp_dist;
  x_max += interp_dist;
  y_max += interp_dist;

  /* Compute the length of bounding box in each direction */
  xL = x_max - x_min; 
  yL = y_max - y_min;

  /* nnx and nny are the number of bins in each direction */
  nnx = (int) xL/interp_dist;
  nny = (int) yL/interp_dist;

  /* Restrict the total number of bins */
  nnx = MIN(nnx, MAX_NNX);
  nny = MIN(nny, MAX_NNY);

  /* allocate hash */
  nodes_hash = (NODES_ENTRY ***) malloc(sizeof(NODES_ENTRY **) * nnx);
  for (ii = 0; ii < nny; ii++)
    {
      nodes_hash[ii] = (NODES_ENTRY **) malloc(sizeof(NODES_ENTRY *) * nny);
      for (jj = 0; jj < nny; jj++)
        {
          nodes_hash[ii][jj] = malloc(sizeof(NODES_ENTRY));
          nodes_hash[ii][jj] = NULL;
        }
    }

  /* READ THE SCATTER DATA */

  /* First, count the number of entries 
   * (which is the number of lines minus 1)
   */

  numscat = -1;
  while(fgets(line, MAXLINE, fp_scatter) != NULL) 
    {
      numscat++;
    }
  rewind (fp_scatter);

  /* Read the scatter points */
  scatter_node = malloc(sizeof(VECT) * numscat);
  fgets(line, MAXLINE, fp_scatter);
  inode = 0;
  while(fgets(line, MAXLINE, fp_scatter) != NULL) 
    {
      sscanf(line, "%lf %lf %lf", &scatter_node[inode].x, &scatter_node[inode].y, 
          &scatter_node[inode].z);
      /* Put node in hash */
      index_x = (int) ( nnx * (scatter_node[inode].x - x_min)/xL);
      index_y = (int) ( nny * (scatter_node[inode].y - y_min)/yL);

      /* keep indices within range 
       * Outliers will fall into bins on 'edge'
       */
      index_x = MAX(0, index_x);
      index_x = MIN(nnx - 1, index_x);
      index_y = MAX(0, index_y);
      index_y = MIN(nny - 1, index_y);

      Push_nodes_hash_entry(&nodes_hash[index_x][index_y], inode);

      inode++;
    }


  /* PERFORM INTERPOLATION */

  /* Initialize the nodal elevations */
  for (ii = 0; ii < nnode; ii++)
    {
      node[ii].z = 0.0;
    }

  /* Loop over each node in the ADH mesh */
  for (inode = 0; inode < nnode; inode++)
    {
      /* initialize */
      denom = 0.0;
      numer = 0.0;

      /* find the location in hash */
      index_x = (int) ( nnx * (node[inode].x - x_min)/xL);
      index_y = (int) ( nny * (node[inode].y - y_min)/yL);

      /* search in the 9-cell stencil around the point */
      ix_start = MAX(index_x - 1, 0);
      ix_end =   MIN(index_x + 1, nnx - 1);
      iy_start = MAX(index_y - 1, 0);
      iy_end =   MIN(index_y + 1, nny - 1);

      found = 0;
      for (ii = ix_start; ii <= ix_end; ii++)
        {
          for (jj = iy_start; jj <= iy_end; jj++)
            {
              entry = nodes_hash[ii][jj];
              while (entry != NULL)
                {
                  index = entry->node_number;
                  /* compute distance */
                  distance = pow(node[inode].x - scatter_node[index].x, 2) +
                    pow(node[inode].y - scatter_node[index].y, 2);
                  distance = pow(distance, 0.5) + SMALL8;
                  if (distance <= interp_dist)
                    {
                      found = 1;
                      denom += 1.0/pow(distance, 2);
                      numer += scatter_node[index].z/pow(distance, 2);
                    }
                  
                  entry = entry->next;
                }
            }
        }
      if (found == 0)
        {
          /* We didn't find any points within the search radius; 
           * Now expand the search to include points from further away
           */
          index_x = (int) ( nnx * (node[inode].x - x_min)/xL);
          index_y = (int) ( nny * (node[inode].y - y_min)/yL);

          ix_start = MAX(index_x - 50, 0);
          ix_end =   MIN(index_x + 50, nnx - 1);
          iy_start = MAX(index_y - 50, 0);
          iy_end =   MIN(index_y + 50, nny - 1);
          found = 0;
          for (ii = ix_start; ii <= ix_end; ii++)
            {
              for (jj = iy_start; jj <= iy_end; jj++)
                {
                  entry = nodes_hash[ii][jj];
                  while (entry != NULL)
                    {
                      index = entry->node_number;
                      /* compute distance */
                      distance = pow(node[inode].x - scatter_node[index].x, 2) +
                        pow(node[inode].y - scatter_node[index].y, 2);
                      distance = pow(distance, 0.5) + SMALL8;
                      if (distance <= FACTOR * interp_dist)
                        {
                          found = 1;
                          denom += 1.0/pow(distance, 2);
                          numer += scatter_node[index].z/pow(distance, 2);
                        }
                      entry = entry->next;
                    }
                }
            }
        }
      if (found == 0)
        {
          /* Catastrophic error --- we didn't find any scatter points for interpolation */
          printf("WARNING! No interpolation performed for node #%d\n", inode + 1);
        }
      node[inode].z = numer/denom;
    }

  for (ii = 0; ii < nelem2d; ii ++)
   {
     fprintf(fp_out, "E3T %d %d %d %d %d\n",ii+1, elem2d[ii].nodes[0], elem2d[ii].nodes[1],elem2d[ii].nodes[2], elem2d[ii].mat);
   }
  for (ii = 0; ii < nnode; ii++)
    {
      fprintf(fp_out, "ND %d %lf %lf %lf\n", ii + 1, node[ii].x, node[ii].y, node[ii].z);
    }

}



int Push_nodes_hash_entry(NODES_ENTRY ** headref, int inode)
{
  NODES_ENTRY *newNode = NULL;
  newNode = (NODES_ENTRY *) malloc(sizeof(NODES_ENTRY));
  newNode->node_number = inode;
  newNode->next = *headref;
  *headref = newNode;
  return;
}

 int init_split(
  char *line,			/* the input line */
  char *data			/* the data */
)
{
  int ipmc;			/* i + MAXCARD */
  int card;			/* the card value */
  int card1, card2, card3;	/* temporary holders for the three card values */

  /* reads the card */
  /* for each card entry we assign it a number based on the following:
     (' ':0), (a-z:1-26), (A-Z:1-26), (0-9:27-36)

     the integer card value is given by:
     1369*i1+37*i2+i3
   */
  card1 = line[0];
  card2 = line[1];
  card3 = line[2];
  if(card1 >= 'a' && card1 <= 'z')
    card1 = card1 - 'a' + 1;
  else if(card1 >= 'A' && card1 <= 'Z')
    card1 = card1 - 'A' + 1;
  else if(card1 >= '0' && card1 <= '9')
    card1 = card1 - '0' + 27;
  else
    card1 = 0;
  if(card2 >= 'a' && card2 <= 'z')
    card2 = card2 - 'a' + 1;
  else if(card2 >= 'A' && card2 <= 'Z')
    card2 = card2 - 'A' + 1;
  else if(card2 >= '0' && card2 <= '9')
    card2 = card2 - '0' + 27;
  else
    card2 = 0;
  if(card3 >= 'a' && card3 <= 'z')
    card3 = card3 - 'a' + 1;
  else if(card3 >= 'A' && card3 <= 'Z')
    card3 = card3 - 'A' + 1;
  else if(card3 >= '0' && card3 <= '9')
    card3 = card3 - '0' + 27;
  else
    card3 = 0;
  card = 1369 * card1 + 37 * card2 + card3;

  /* sets the data array */
  for(ipmc = MAXCARD; ipmc < MAXLINE; ipmc++)
    {
      if(line[ipmc] == '!')
	{
	  data[ipmc - MAXCARD] = '\0';
	  return (card);
	}
      else if(line[ipmc] == '\0')
	{
	  data[ipmc - MAXCARD] = '\0';
	  return (card);
	}
      else
	{
	  data[ipmc - MAXCARD] = line[ipmc];
	}
    }

  /* if it drops out the bottom, then the line is too long */
 /* init_read_error(line, "Bad input line in init_split - too long.");*/
  return (UNSET_INT);
}


double init_read_ddatum(
  char *line,			/* the original line */
  char *data			/* the data */
)
{
  int i, j;			/* loop counter */
  int icnt;			/* white space */
  char datum[MAXLINE];		/* holds the datum */
  double value;			/* holds the value */

  /* initializes the datum */
  for(i = 0; i < MAXLINE; i++)
    datum[i] = '\0';

  /* strips the initial white space */
  icnt = 0;
  while((data[icnt] == ' ' || data[icnt] == '\n' || data[icnt] == '\t' || data[icnt] == '\v'
	 || data[icnt] == '\r' || data[icnt] == '\f') && icnt < MAXLINE)
    icnt++;
  for(i = 0; i < MAXLINE - icnt; i++)
    data[i] = data[i + icnt];

  /* checks that the line has not ended */
  if(data[0] == '\0' || icnt == MAXLINE)
    printf("Bad read error in init_read_ddatum.");

  /* puts the datum into the datum aray */
  i = 0;
  while((data[i] == '0' || data[i] == '1' || data[i] == '2' || data[i] == '3' ||
	 data[i] == '4' || data[i] == '5' || data[i] == '6' || data[i] == '7' ||
	 data[i] == '8' || data[i] == '9' || data[i] == '.' || data[i] == '-' ||
	 data[i] == '+' || data[i] == 'e' || data[i] == 'E' || data[i] == 'g' ||
	 data[i] == 'G') && i < MAXLINE)
    {
      datum[i] = data[i];
      i++;
    }

  /* checks that the next data is white space */
  if(data[i] != ' ' && data[i] != '\n' && data[i] != '\t' && data[i] != '\v' &&
     data[i] != '\r' && data[i] != '\f')
    printf("Bad double in init_read_ddatum.");

  /* peels off the data */
  for(j = 0; j < MAXLINE - i; j++)
    data[j] = data[i + j];

  /* reads the value */
  sscanf(datum, "%lf", &value);

  /* returns the value */
  return (value);
}


int init_read_idatum(
  char *line,			/* the original line */
  char *data			/* the data */
)
{
  int i, j;			/* loop counter */
  int icnt;			/* white space */
  char datum[MAXLINE];		/* holds the datum */
  int ivalue;			/* holds the value */

  /* initializes the datum */
  for(i = 0; i < MAXLINE; i++)
    datum[i] = '\0';

  /* strips the initial white space */
  icnt = 0;
  while((data[icnt] == ' ' || data[icnt] == '\n' || data[icnt] == '\t' || data[icnt] == '\v'
	 || data[icnt] == '\r' || data[icnt] == '\f') && icnt < MAXLINE)
    icnt++;
  for(i = 0; i < MAXLINE - icnt; i++)
    data[i] = data[i + icnt];

  /* checks that the line has not ended */
  if(data[0] == '\0' || icnt == MAXLINE)
    printf("Bad read error in init_read_idatum.\n");

  /* puts the datum into the datum aray */
  i = 0;
  while((data[i] == '0' || data[i] == '1' || data[i] == '2' || data[i] == '3' ||
	 data[i] == '4' || data[i] == '5' || data[i] == '6' || data[i] == '7' ||
	 data[i] == '8' || data[i] == '9' || data[i] == '-') && i < MAXLINE)
    {
      datum[i] = data[i];
      i++;
    }

  /* checks that the next data is white space */
  if(data[i] != ' ' && data[i] != '\n' && data[i] != '\t' && data[i] != '\v' &&
     data[i] != '\r' && data[i] != '\f')
    printf("Bad data in init_read_idatum.");

  /* peels off the data */
  for(j = 0; j < MAXLINE - i; j++)
    data[j] = data[i + j];

  /* reads the value */
  sscanf(datum, "%d", &ivalue);

  /* returns the value */
  return (ivalue);
}



