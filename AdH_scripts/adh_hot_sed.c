#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include <time.h>
#include <stdlib.h>
#include <stddef.h>
#include <malloc.h>

int main
(
 int argc,
 char *argv[]
)
{
  FILE *fp_list, *fp_in, *fp_out;
  char filename[100], fileout[100], line[100], dummy[80], dataname[80], **dataline;
  float savetime, currtime, prnttime;
  int nnode, i, itmp, number;

  if(argc != 2)
    {
    fprintf(stderr,"Usage: adh_hot <hotstart definition file>\n");
    exit(0);
    }
  else
    {
    /* *** Open Hotstart Definition File *** */
    if((fp_list = fopen(argv[1],"r")) == NULL){
      fprintf(stderr, "Can't open file %s mode r. \n", argv[2]);
      exit(0);
    }
    else{
      printf("Reading Hotstart Definition File: %s\n",argv[1]);
    }
    /* ***  Read Time To Save To Hotstart And Output File Name *** */
    if(fgets(line,100,fp_list) != NULL){
      sscanf(line,"%e %s",&savetime,fileout);
    }
    else{
      fprintf(stderr,"Error in hotstart definition file. \n");
      exit(0);
    }
    /* *** Open Output Hotstart File *** */
    if((fp_out = fopen(fileout,"w")) == NULL){
      fprintf(stderr, "Can't open file %s mode w. \n",fileout);
      exit(0);
    }
    /* ***  Read Files *** */
    while(fgets(line,100,fp_list) != NULL)
      {
      number = 99999;
      /**sscanf(line,"%s %s",filename,dataname);**/
      /*** will sscanf stop at end of line if third variable isn't there??? ***/
      sscanf(line,"%s %s %d",filename,dataname,&number);
      if((fp_in = fopen(filename,"r")) == NULL){
        fprintf(stderr, "Can't open file %s mode r. \n",filename);
        exit(0);
      }
      else{
        printf("Reading File: %s\n",filename);
      }
      while((fgets(line,100,fp_in) != NULL) && (strncmp(line,"TS",2) != 0)){
        if(strncmp(line,"NAME",4)==0){
          if(number != 99999){
            fprintf(fp_out,"NAME %s  %d\n",dataname,number);
          }
          else{
            fprintf(fp_out,"NAME %s \n",dataname);
          }
        }
        else{
          fprintf(fp_out,"%s",line);
        }
        if(strncmp(line,"ND",2)==0){
          sscanf(line,"%s %d",dummy,&nnode);
          /* isize = sizeof(dataline)*nnode; */
          dataline = malloc(sizeof(*dataline)*nnode);
          for(i = 0; i < nnode; ++i) dataline[i] = malloc(sizeof(char)*200);
        }
      }
      do {
        sscanf(line,"%s %d %f",dummy,&itmp,&currtime);
        if(currtime <= savetime){
          prnttime=currtime;
          for(i = 0; i < nnode; i++){
            fgets(dataline[i],200,fp_in);
          }
        }
      } while((fgets(line,100,fp_in) != NULL) && (currtime < savetime));

      printf("Desired hotstart time: %e \n",savetime);
      printf("  Saved hotstart time: %e \n",prnttime);
      printf("Number of Nodes: %d\n",nnode);

      fprintf(fp_out, "TS 0 %15.8e\n", prnttime);
      for(i = 0; i < nnode; i++){
        fprintf(fp_out,"%s",dataline[i]);
      }
      fprintf(fp_out,"ENDDS\n");

      fclose(fp_in);
      for(i = 0; i < nnode; ++i) free(dataline[i]);
      free(dataline);
    }
  }
  return(0);
}
