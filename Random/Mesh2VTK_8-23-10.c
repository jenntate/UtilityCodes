/** Mesh2VTK.c
	C code written by Kevin Winters (based on code by Amanda Hines)

	This utility takes a mesh file (*.1dm, *.2dm or *.3dm) and ASCII solution files (*.dat) and converts them to VTK files
	compatiable with the visualization tool ParaView. Solution files are assumed to include timesteps of matching
	frequency (1st solution's 1st timestep has the same time value of the 2nd solution's 1st timestep and so forth).

    The executable should be called with the name of the mesh file (required) and a list of the solution files properties (optional).
	Output will be written to the directory containing the mesh file or to the directory containing the first solution file (if
	specified). File paths and names that include spaces must be specified within quotes.
		
		./Mesh2VTK.exe mesh_file [solution_1] [solution_2] ... [solution_n] [-f file_format] [-o output_file_name]

		usage:		./Mesh2VTK.exe sample.3dm
		results:	/sample_3d.vtu
		
		usage:		./Mesh2VTK.exe "sample mesh.3dm"
		results:	/sample mesh 3d.vtu

		usage:		./Mesh2VTK.exe sample.3dm sample_phd.dat sample.vel
		results:	/sample_3d.pvd, /sample_3d/sample_3d.0.vtu, /sample_3d/sample_3d.1.vtu, ...

	Optional argument flags:

	-f file_format

		Specify "vtk" or "vtu" to output information in VTK Legacy file foramt (*.vtk) or VTK XML Unstructured Grid file
		format (*.vtu), respectively. The file format is defaulted to *.vtu when this flag is not specified. When multiple
		*.vtu files (one for each timestep) are written, a ParaView Data File (*.pvd) that contains timestep information is
		also written. The VTK Legacy file format does not provide timestep information (the actual time value).

		usage:		./Mesh2VTK.exe sample.3dm -f vtk
		results:	/sample_3d.vtk

		usage:		./Mesh2VTK.exe sample.3dm sample_phd.dat sample.vel -f vtk
		results:	/sample_3d/sample_3d.0.vtk, /sample_3d/sample_3d.1.vtk, ...

		usage:		./Mesh2VTK.exe sample.3dm sample_phd.dat sample.vel -f vtu
		results:	/sample_3d.pvd, /sample_3d/sample_3d.0.vtu, /sample_3d/sample_3d.1.vtu, ...

	-o output_file_name
		
		Specify the file base name of the output (file path and extension is not specified). Output file names that include spaces
		must be specified within quotes. The output file name is defaulted to the mesh file's base name with the dimension appended
		when this flag is not specified.
		
		usage:		./Mesh2VTK.exe sample.3dm -o output
		results:	/output.vtu

		usage:		./Mesh2VTK.exe sample.3dm -o "output data"
		results:	/output data.vtu
		
		usage:		./Mesh2VTK.exe sample.3dm sample_phd.dat sample.vel -o output
		results:	/output.pvd, /output/output.0.vtu, /output/output.1.vtu, ...

	Additional information:

	The mesh file must be in:
		xMS/ADH *.3dm file format
			Some related documentaion is available at http://www.xmswiki.com/xms/SMS:2D_Mesh_Files_%28*.2dm%29
			Geometry cards expected/handled by this utility:
				MESH3D (file declaration - first card in file)
				E3T (2D triangle with 3 nodes)
				E4Q (2D quadrilateral with 4 nodes)
				E6T (2D quadrautic triangle with 6 nodes)
				E8Q (2D quadrautic quadrilateral with 8 nodes)
				E4T (3D tetrathedron with 4 nodes)
				E5P (3D pyramid with 5 nodes)
				E6W (3D wedge or prism with 6 nodes)
				E8H (3D hexahedron with 8 nodes)
				ND (node)
	or
		WASH123D *.1dm/*.2dm/*.3dm file format
			Geometry cards expected/handled by this utility:
				WMS1DM/WMS2DM/WMS3DM (file declaration - first card in file)
				GE2 (1D line with 2 nodes)
				GE3 (2D triangle with 3 nodes)
				GE4 (2D quadrilateral with 4 nodes) (the file declaration card determines the dimension)
				GE4 (3D tetrathedron with 4 nodes) (the file declaration card determines the dimension)
				GE6 (3D wedge or prism with 6 nodes)
				GE8 (3D hexahedron with 8 nodes)
				GN (node)

	Solution files must be in:
		xMS/ADH/WASH123D ASCII *.dat file format
			Documentation is available at http://wikis.aquaveo.com/xms/SMS:ASCII_Data_Set_Files_%28*.dat%29
			Cards expected by this utility:
				ND (number of mesh nodes)
				NC (number of mesh elements)
				OBJTYPE (object type)
				BEGSCL or BEGVEC (specifies the type of data, scalar or vector, respectively)
				NAME (name of the dataset)
				TS (start of timestep data)
				ENDDS (end of timestep data)
*/

#include <stdlib.h>
#include <stdio.h>

#include <ctype.h>
#include <string.h>
#include <math.h>

#define STRLEN 256

enum ArgOptions { ARG_UNKNOWN, ARG_FORMAT, ARG_OUTPUT };
enum FileFormats { FF_VTK, FF_VTU };
enum DatasetFileCard { DS_UNKNOWN, DS_NAME, DS_TS, DS_BEGSCL, DS_BEGVEC, DS_ND, DS_NC, DS_OBJTYPE, DS_ENDDS };

typedef struct {
    double m_x, m_y, m_z;       /* the coordinates of the vector */
	int m_nread;				/* number times read from file */
} NODE;

typedef struct {
    int m_nnodes;               /* number of nodes (element type) */
    int m_vtktype;              /* vtk cell type */
    int m_mat;                  /* material */
    int *m_nodes;               /* the nodes in the element */
	int m_nread;				/* number times read from file */
} ELEM;

int g_fileformat = FF_VTU;
int g_nnodes = 0;
int g_nelemnoderefs = 0;
int g_nelems = 0;
int g_dimension = 0;
int g_nTimesteps = 0;
NODE *g_nodes;
ELEM *g_elems;

void preview_mesh_file(char *a_filename, int *a_nnodes, int *a_nelems);
void read_mesh_file(char *a_filename);
void write_vtk_file(char *a_filebasename, char *a_header_note);
void write_vtu_file_start(char *a_filebasename, char *a_header_note);
void write_vtu_file_end(char *a_filebasename);
void write_pvd_file(char *a_filebasename, char *a_dataset_name);
void convert_dat_file(char *a_filebasename, char *a_datfilename, int a_firstdatfile);
static int get_arg_type(char *a_card);
static int get_file_format_type(char *a_card);
static int get_dat_file_card_type(char *a_card);
void make_lower_case(char *a_str);
void trim_spaces(char *a_str);
void replace_spaces(char *a_str);
void initialize_int_array(int a_length, int *a_array);
void initialize_node_array(int a_length, NODE *a_array);
void initialize_elem_array(int a_length, ELEM *a_array);

int main(int a_argc, char *a_argv[])
{
	char strTmp[STRLEN] = "", strBasename[STRLEN] = "", strPath[STRLEN] = "", strPathAndBase[STRLEN] = "",
		 cSlash = '/', *cp = NULL;
    int ii = 0, jj = 0, iMesh = 0, iOutput = 0, *iDatasets = NULL;
	
	printf("\n\nMesh2VTK - Compiled on %s\n\n", __DATE__);
	
	/* sort and validate arguments */
	if (a_argc > 1) {
		iDatasets = (int *)malloc(sizeof(int) * a_argc);
		initialize_int_array(a_argc, iDatasets);
	
		for (ii = 1; ii < a_argc; ++ii) {
			strcpy(strTmp, a_argv[ii]);
			switch (get_arg_type(strTmp)) {
				case ARG_FORMAT:
					if (ii + 1 < a_argc)
						g_fileformat = get_file_format_type(a_argv[++ii]);
					break;
				case ARG_OUTPUT:
					if (ii + 1 < a_argc)
						iOutput = ++ii;
					break;
				case ARG_UNKNOWN:
				default:
					if (iMesh == 0)
						iMesh = ii;
					else {
						for (jj = 0; jj < a_argc && iDatasets[jj] != 0; ++jj); /* empty loop */
						iDatasets[jj] = ii;
					}
					break;
			}
		}
	}
	if (iMesh == 0) {
        printf("Specify a mesh geometry file (*.1dm, *.2dm, *.3dm format) as 1st argument followed by any number of "
			"optional dataset files (*.dat format), i.e.:\n ./Mesh2VTK.exe sample.3dm sample_phd.dat sample.vel\n\n");
        return 0;
	}
	
    /* read the original mesh */
    read_mesh_file(a_argv[iMesh]); /* this sets the geometry dimension */
	
    /* retrieve filename base (no path)*/
	if (iOutput != 0)
		strcpy(strTmp, a_argv[iOutput]);
	else
		strcpy(strTmp, a_argv[iMesh]);
	cp = strtok(strTmp, "/\\");
	while (cp) {
		strcpy(strBasename, cp);
		cp = strtok(NULL, "/\\");
	}
	if (iOutput == 0) {
		cp = strrchr(strBasename, '.');
		if (cp)
			*cp = '\0';
		/* create output filename base (add the dimension so a WASH123D with different components won't overwrite each other) */
		cp = strrchr(strBasename, ' ');
		sprintf(strBasename, "%s%c%dd", strBasename, cp ? ' ' : '_', g_dimension);
	}
	
	/* retrieve path */
	if (!iDatasets || iDatasets[0] == 0)
		/* use path of the mesh */
		strcpy(strPath, a_argv[iMesh]);
	else
		/* use path of the first dataset */
		strcpy(strPath, a_argv[iDatasets[0]]);
	/* determine which path delimiter is used, '\' or '/' */
	cSlash = strPath[strcspn(strPath, "/\\")];
	if (cSlash != '\0')
		strrchr(strPath, cSlash)[1] = '\0';
	else {
		cSlash = '/';
		strcpy(strPath, "");
	}
	
	/* add path and base */
	sprintf(strPathAndBase, "%s%s", strPath, strBasename);
	
    /* convert the solution files and mesh file to vtk files */
    printf("Starting conversion...\n\n");
	if (!iDatasets || iDatasets[0] == 0) {
		/* only convert mesh */
		switch (g_fileformat) {
			case FF_VTK:
				write_vtk_file(strPathAndBase, "");
				break;
			case FF_VTU:
			default:
				write_vtu_file_start(strPathAndBase, "");
				write_vtu_file_end(strPathAndBase);
				break;
		}			
		printf("Geometry was written to %s\n", strPathAndBase);
	}
	else {
		char strDSPathAndBase[STRLEN] = "";

		/* attempt to make directory using proper file path "/" syntax */
		printf("Checking for directory %s\n", strPathAndBase);
		sprintf(strTmp, "mkdir \"%s\"", strPathAndBase);
		/* if this system call fails then assume that the directory already exists */
		/* "mkdir" could fail if its not a system call (for a specialty OS) */
		if (system(strTmp) == 0)
			printf("Created directory %s\n", strPathAndBase);
		printf("\n");
		sprintf(strDSPathAndBase, "%s%c%s", strPathAndBase, cSlash, strBasename);
		
		/* read in the solution files and create vtk files for each timestep */
		ii = 0;
		while (iDatasets[ii]) {
			convert_dat_file(strDSPathAndBase, a_argv[iDatasets[ii]], ii == 0);
			++ii;
		}
		
		if (g_fileformat == FF_VTU) {
			write_vtu_file_end(strDSPathAndBase);
			write_pvd_file(strPathAndBase, strBasename);
		}
	}

	printf("\nMesh2VTK has finished\n\n");

	/* clean up */
	free(iDatasets);
	free(g_nodes);
	for (ii = 0; ii < g_nelems; ++ii)
		free(g_elems[ii].m_nodes);
	free(g_elems);
    return 0;
}

/*!
   \brief Initializes all entries of a (int) array to zero
 */
void initialize_int_array(int a_length, int *a_array)
{
	int i = 0;
	
    for (i = 0; i < a_length; ++i)
        a_array[i] = 0;
}

/*!
 \brief Initializes all entries of a (int) array to zero
 */
void initialize_node_array(int a_length, NODE *a_array)
{
	int i = 0;
	
    for (i = 0; i < a_length; ++i)
		a_array[i].m_x = a_array[i].m_y = a_array[i].m_z = a_array[i].m_nread = 0;
}

/*!
 \brief Initializes all entries of a (int) array to zero
 */
void initialize_elem_array(int a_length, ELEM *a_array)
{
	int i = 0;
	
    for (i = 0; i < a_length; ++i) {
		a_array[i].m_mat = a_array[i].m_nnodes = a_array[i].m_vtktype = a_array[i].m_nread = 0;
		a_array[i].m_nodes = NULL;
	}
}

/*!
   \Write Mesh in vtk format
 */
void write_vtk_file(char *a_filebasename, char *a_header_note)
{
    int i = 0, j = 0;
    FILE *fpw = NULL;

    if (a_filebasename == NULL)
        return;
	strcat(a_filebasename, ".vtk");
    fpw = fopen(a_filebasename, "w");
    if (fpw == NULL) {
		printf("* Error writing file: %s\n\nExiting\n\n", a_filebasename);
        exit(0);
    }
    /*Writing the data to the file in legacy vtk format */
    /*Writing the header information as well as the number of nodes */
    fprintf(fpw, "# vtk DataFile Version 3.0\nWritten by Mesh2VTK %s\nASCII\nDATASET UNSTRUCTURED_GRID\nPOINTS %d double\n",
            a_header_note, g_nnodes);
    /*writing the nodes to the file */
    for (i = 0; i < g_nnodes; i++)
        fprintf(fpw, "%16.15le %16.15le %16.15le\n", g_nodes[i].m_x, g_nodes[i].m_y, g_nodes[i].m_z);
    /*Writing the elements to the file */
    if (g_nelems != 0) {
		/* write elements */
        fprintf(fpw, "\nCELLS %d %d\n", g_nelems, g_nelems + g_nelemnoderefs);
        for (i = 0; i < g_nelems; ++i) {
            fprintf(fpw, "%d ", g_elems[i].m_nnodes);
			for (j = 0; j < g_elems[i].m_nnodes; ++j)
				fprintf(fpw, "%d ", g_elems[i].m_nodes[j]);
			fprintf(fpw, "\n");
		}
		/* write element vtk types*/
        fprintf(fpw, "\nCELL_TYPES %d\n", g_nelems);
        for (i = 0; i < g_nelems; i++)
            fprintf(fpw, "%d\n", g_elems[i].m_vtktype);
		/* write element IDs as dataset */
        fprintf(fpw, "\nCELL_DATA %d\nSCALARS Mesh_Elem_IDs int 1\nLOOKUP_TABLE default\n", g_nelems);
        for (i = 0; i < g_nelems; i++)
            fprintf(fpw, "%d\n", i + 1);
		/* write element materials as dataset */
        fprintf(fpw, "\nSCALARS Mesh_Mat_IDs int 1\nLOOKUP_TABLE default\n");
        for (i = 0; i < g_nelems; i++)
            fprintf(fpw, "%d\n", g_elems[i].m_mat);
    }
	else {
		/* write points as vertex elements */
        fprintf(fpw, "\nCELLS %d %d\n%d ", g_nnodes, g_nnodes + 1, g_nnodes);
		for (i = 0; i < g_nnodes; ++i)
			fprintf(fpw, "%d ", i);
		/* write element vtk types*/
        fprintf(fpw, "\n\nCELL_TYPES 1\n2\n");
	}
	/* write node IDs as a dataset */
	fprintf(fpw, "\nPOINT_DATA %d\nSCALARS Mesh_Node_IDs int 1\nLOOKUP_TABLE default\n", g_nnodes);
    for (i = 0; i < g_nnodes; ++i)
        fprintf(fpw, "%d\n", i + 1);
    fprintf(fpw, "\n");
    fclose(fpw);
    return;
}

/*!
   \brief Write beginning of vtk format
 */
void write_vtu_file_start(char *a_filebasename, char *a_header_note)
{
    int ii = 0, jj = 0, iTmp = 0;
    FILE *fpw = NULL;

    if (!a_filebasename)
        return;
	strcat(a_filebasename, ".vtu");
    fpw = fopen(a_filebasename, "w");
    if (fpw == NULL) {
		printf("* Error writing file: %s\n\nExiting\n\n", a_filebasename);
        exit(0);
    }

    /* write the mesh to the file in vtk xml unstructured grid format */
    /* write header comment and open main xml tags */
    fprintf(fpw, "<?xml version=\"1.0\"?>\n<!--Written by Mesh2VTK%s%s-->\n"
		"<VTKFile type=\"UnstructuredGrid\" version=\"0.1\" byte_order=\"LittleEndian\">\n"
		"<UnstructuredGrid>\n<Piece NumberOfPoints=\"%d\" NumberOfCells=\"%d\">\n",
		strlen(a_header_note) ? " " : "", a_header_note, g_nnodes, g_nelems > 0 ? g_nelems : 1);

    /* write the elements to the file */
    if (g_nelems != 0) {
		/* write elements */
        fprintf(fpw, "<Cells>\n<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
        for (ii = 0; ii < g_nelems; ++ii) {
			for (jj = 0; jj < g_elems[ii].m_nnodes; ++jj)
				fprintf(fpw, "%d%s", g_elems[ii].m_nodes[jj], jj < g_elems[ii].m_nnodes - 1 ? " " : "");
			fprintf(fpw, "\n");
		}
        fprintf(fpw, "</DataArray>\n<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n");
        for (ii = 0, iTmp = 0; ii < g_nelems; ++ii) {
			iTmp += g_elems[ii].m_nnodes;
			fprintf(fpw, "%d\n", iTmp);
		}
        fprintf(fpw, "</DataArray>\n<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n");
        for (ii = 0; ii < g_nelems; ++ii)
			fprintf(fpw, "%d\n", g_elems[ii].m_vtktype);
        fprintf(fpw, "</DataArray>\n</Cells>\n");
		
		/* write the element datasets to the file */
		fprintf(fpw, "<CellData Scalars=\"Mesh Elem IDs\">\n"
				"<DataArray type=\"Int32\" Name=\"Mesh Elem IDs\" NumberOfComponents=\"1\" format=\"ascii\">\n");
		for (ii = 0; ii < g_nelems; ++ii)
			fprintf(fpw, "%d\n", ii + 1);
		fprintf(fpw, "</DataArray>\n"
				"<DataArray type=\"Int32\" Name=\"Mesh Mat IDs\" NumberOfComponents=\"1\" format=\"ascii\">\n");
		for (ii = 0; ii < g_nelems; ++ii)
			fprintf(fpw, "%d\n", g_elems[ii].m_mat);
		fprintf(fpw, "</DataArray>\n</CellData>\n");
    }
	else {
		/* write points as vertex elements */
        fprintf(fpw, "<Cells>\n<DataArray type=\"Int32\" Name=\"connectivity\" format=\"ascii\">\n");
        for (ii = 0; ii < g_nnodes; ++ii)
			fprintf(fpw, "%d ", ii);
        fprintf(fpw, "\n</DataArray>\n<DataArray type=\"Int32\" Name=\"offsets\" format=\"ascii\">\n%d\n"
			"</DataArray>\n<DataArray type=\"UInt8\" Name=\"types\" format=\"ascii\">\n2\n</DataArray>\n</Cells>\n", g_nnodes);
	}
	
    /* write the nodes to the file */
	fprintf(fpw, "<Points>\n<DataArray type=\"Float64\" Name=\"Points\" NumberOfComponents=\"3\" format=\"ascii\">\n");
    for (ii = 0; ii < g_nnodes; ++ii)
		fprintf(fpw, "%16.15le %16.15le %16.15le\n", g_nodes[ii].m_x, g_nodes[ii].m_y, g_nodes[ii].m_z);
	fprintf(fpw, "</DataArray>\n</Points>\n");
	
	/* write the node datasets to the file */
	fprintf(fpw, "<PointData Scalars=\"Mesh Node IDs\">\n"
		"<DataArray type=\"Int32\" Name=\"Mesh Node IDs\" NumberOfComponents=\"1\" format=\"ascii\">\n");
	for (ii = 0; ii < g_nnodes; ++ii)
		fprintf(fpw, "%d\n", ii + 1);
	fprintf(fpw, "</DataArray>\n");
	
	/* leave the "PointData" xml tag open to allow for appending of solution datasets */
	/* leave the main xml tags open also */
    
	/* clean up */
	fclose(fpw);	
	
    return;
}

/*!
 \brief Write ending of vtk format
 */
void write_vtu_file_end(char *a_filebasename)
{
	char strFilename[STRLEN] = "";
    FILE *fpw = NULL;
	int ii = 0;
	
    if (!a_filebasename)
        return;
	
	/* finish the XML files */
	for (ii = 0; ii < g_nTimesteps || (ii == 0 && g_nTimesteps == 0); ++ii) {
		if (g_nTimesteps == 0)
			strcpy(strFilename, a_filebasename);
		else 
			sprintf(strFilename, "%s.%d.vtu", a_filebasename, ii);
		fpw = fopen(strFilename, "a");
		if (!fpw) {
			printf("* Error writing file: %s\n\nExiting\n\n", strFilename);
			return;
		}
		
		/* close main xml tags */
		fprintf(fpw, "</PointData>\n</Piece>\n</UnstructuredGrid>\n</VTKFile>\n");
		
		/* clean up */
		fclose(fpw);
	}    
	
	return;
}

/*!
   \brief Write pvd file
 */
void write_pvd_file(char *a_filebasename, char *a_dataset_name)
{
	char strFilename[STRLEN] = "", strDataset[STRLEN] = "";
	double dTS = 0.0;
	int ii = 0, iTmp = 0;
    FILE *fpw = NULL, *fpr = NULL;

	if (!a_filebasename || !a_dataset_name)
		return;

	sprintf(strFilename, "%s.pvd", a_filebasename);
    fpw = fopen(strFilename, "w");
    if (fpw == NULL) {
		printf("* Error writing file: %s\n\nExiting\n\n", strFilename);
        exit(0);
    }

    /* write the dataset collection to the file in vtk xml format */
    /* write header comment and open main xml tags */
    fprintf(fpw, "<?xml version=\"1.0\"?>\n<!--Written by Mesh2VTK-->\n"
		"<VTKFile type=\"Collection\" version=\"0.1\" byte_order=\"LittleEndian\">\n<Collection>\n");
	

	for (ii = 0; ii < g_nTimesteps; ++ii) {
		sprintf(strDataset, "%s/%s.%d.vtu", a_filebasename, a_dataset_name, ii);
		fpr = fopen(strDataset, "r");
		if (fpr) {
			iTmp = fscanf(fpr, "%*[^:]%*c %lf", &dTS);
			fprintf(fpw, "<DataSet timestep=\"%lf\" group=\"%s\" part=\"0\" file=\"%s/%s.%d.vtu\"/>\n",
					dTS, a_dataset_name, a_dataset_name, a_dataset_name, ii);
			fclose(fpr);
			fpr = NULL;
		}
	}
	
	/* close main xml tags */
    fprintf(fpw, "</Collection>\n</VTKFile>\n");
	
	/* clean up */
	fclose(fpw);
	
	printf("\nDataset file information written to %s\n", strFilename);

	return;
}

/*!
   \brief Counts Elems and Nodes in File
 */
void preview_mesh_file(char *a_filename, int *a_nnodes, int *a_nelems)
{
    char strLine[STRLEN] = "", strCard[STRLEN] = "";
    FILE *fpr = NULL;
	int iLine = 0, i1D = 0, i2D = 0, i3D = 0, iErr = 0;
	
	/* open file */
    fpr = fopen(a_filename, "r");
    if (fpr == NULL) {
		printf("* Error reading file: %s\n\nExiting\n\n", a_filename);
        exit(0);
    }
    printf("Previewing mesh geometry file: %s\n\n", a_filename);
    /* read file */
	if (fgets(strLine, STRLEN, fpr)) {
		make_lower_case(strLine);
		if (strstr(strLine, "1d"))
			g_dimension = 1;
		else if (strstr(strLine, "2d"))
			g_dimension = 2;
		else 
			g_dimension = 3;
	}
	while (!feof(fpr)) {
		if (fgets(strLine, STRLEN, fpr)) {
			++iLine;
			if (sscanf(strLine, "%s", strCard) != 1)
				iErr += printf("* Failed to read file card on line %d\n", iLine);
			if (strcmp(strCard, "E3T") == 0 || strcmp(strCard, "E4Q") == 0 ||
				strcmp(strCard, "E6T") == 0 || strcmp(strCard, "E8Q") == 0)
				++i2D;
			else if (strcmp(strCard, "E4T") == 0 || strcmp(strCard, "E5P") == 0 ||
					 strcmp(strCard, "E6W") == 0 || strcmp(strCard, "E8H") == 0)
				++i3D;
			else if (strCard[0] == 'G' && strCard[1] == 'E') {
				if (strCard[2] == '2')
					++i1D;
				else if (strCard[2] == '3' || (strCard[2] == '4' && g_dimension == 2))
					++i2D;
				else if ((strCard[2] == '4' && g_dimension == 3) || strCard[2] == '6' || strCard[2] == '8')
					++i3D;
			}
			else if (strcmp(strCard, "ND") == 0 || strcmp(strCard, "GN") == 0)
				++(*a_nnodes);			
		}
		else if (ferror(fpr)) {
			iErr += printf("* Error reading file line %d\n", iLine + 1);
			break;
		}
	}
	/* clean up */
    fclose(fpr);
	/* exit early */
	if (iErr != 0) {
		printf("\n* Mesh file must be corrected before conversion can occur\n\nExiting\n\n");
		exit(0);
	}
	/* print info */
    printf("Nodes = %d, 1D Elems = %d, 2D Elems = %d, 3D Elems = %d\n\n", *a_nnodes, i1D, i2D, i3D);
	*a_nelems = i1D + i2D + i3D;
}

/*!
   \brief This function reads in the original 3dm/2dm/1dm mesh.
 */
void read_mesh_file(char *a_filename)
{
    char strLine[STRLEN] = "", strCard[STRLEN] = "", strBase[STRLEN] = "", strFormat[STRLEN] = "";
    double dX = 0.0, dY = 0.0;
    int ii = 0, iTmp = 0, iLine = 0, iID = 0, iErr = 0, iMatWarning = 0;
    FILE *fpr = NULL;
	
	/* open file */
    fpr = fopen(a_filename, "r");
    if (fpr == NULL) {
		printf("* Error reading file: %s\n\nExiting\n\n", a_filename);
        exit(0);
    }
	/* initialize arrays */
    preview_mesh_file(a_filename, &g_nnodes, &g_nelems);
    printf("Loading mesh geometry...\n\n");

    if (g_nnodes > 0) {
        g_nodes = (NODE *)malloc(sizeof(NODE) * g_nnodes);
		initialize_node_array(g_nnodes, g_nodes);
	}
    if (g_nelems > 0) {
        g_elems = (ELEM *)malloc(sizeof(ELEM) * g_nelems);
		initialize_elem_array(g_nelems, g_elems);
	}
	/* read file */
	while (!feof(fpr)) {
		if (fgets(strLine, STRLEN, fpr)) {
			++iLine;
			if (sscanf(strLine, "%s", strCard) != 1)
				iErr += printf("* Failed to read file card on line %d\n", iLine);
			/* read element */
			if (strcmp(strCard, "E3T") == 0 || strcmp(strCard, "E4Q") == 0 || /* GMS 2D elems */
				strcmp(strCard, "E6T") == 0 || strcmp(strCard, "E8Q") == 0 || /* GMS 2D elems */
				strcmp(strCard, "E4T") == 0 || strcmp(strCard, "E5P") == 0 || /* GMS 2D elems */
				strcmp(strCard, "E6W") == 0 || strcmp(strCard, "E8H") == 0 || /* GMS 3D elems */
				strcmp(strCard, "GE2") == 0 || /* WASH 1D elems */
				strcmp(strCard, "GE3") == 0 || strcmp(strCard, "GE4") == 0 || /* WASH 2D elems */
				strcmp(strCard, "GE4") == 0 || strcmp(strCard, "GE6") == 0 || strcmp(strCard, "GE8") == 0) { /* WASH 3D elems */
				/* read element ID */
				if (sscanf(strLine, "%*s %d", &iID) == 1) {
					if (iID > 0 && iID <= g_nelems) {
						++(g_elems[iID - 1].m_nread);
						/* number of nodes from card name */
						if (strCard[0] == 'E')
							g_elems[iID - 1].m_nnodes = atoi(&strCard[1]);
						else if (strCard[0] == 'G')
							g_elems[iID - 1].m_nnodes = atoi(&strCard[2]);
						g_nelemnoderefs += g_elems[iID - 1].m_nnodes;
						/* intialize node ID array */
						g_elems[iID - 1].m_nodes = (int *)malloc(sizeof(int) * g_elems[iID - 1].m_nnodes);
						initialize_int_array(g_elems[iID - 1].m_nnodes, g_elems[iID - 1].m_nodes);
						/* read remaining node IDs */
						strcpy(strBase, "%*s %*d ");
						for (ii = 0; ii < g_elems[iID - 1].m_nnodes; ++ii) {
							strcpy(strFormat, strBase);
							if (sscanf(strLine, strcat(strFormat, "%d"), &iTmp) != 1) {
								iErr += printf("* Failed to read element information (ID, nodes, material) on line %d\n", iLine);
								break;
							}
							else if (iTmp <= 0 || iTmp > g_nnodes) {
								iErr += printf("* Node ID %d read on line %d is outside of expected range (1 to %d)\n",
											   iTmp, iLine, g_nnodes);
								break;
							}
							g_elems[iID - 1].m_nodes[ii] = iTmp - 1;
							strcat(strBase, " %*d");
							/* read material ID after last node ID */
							if (ii == g_elems[iID - 1].m_nnodes - 1 && sscanf(strLine, strcat(strBase, "%d"), &(g_elems[iID - 1].m_mat)) != 1 &&
								iMatWarning == 0) {
								printf("* Element information did not include material type on line %d, all missing materials "
									   "will be defaulted to 0\n", iLine);
								iMatWarning = 1;
							}
						}							
						/* vtk type */		
						if (strcmp(strCard, "E3T") == 0 || strcmp(strCard, "GE3") == 0)
							g_elems[iID - 1].m_vtktype = 5;
						else if (strcmp(strCard, "E4Q") == 0 || (strcmp(strCard, "GE4") == 0 && g_dimension == 2))
							g_elems[iID - 1].m_vtktype = 9;
						else if (strcmp(strCard, "E6T") == 0) {
							g_elems[iID - 1].m_vtktype = 22;
							/* re-order the nodes into vtk standard */
							iTmp = g_elems[iID - 1].m_nodes[1];
							g_elems[iID - 1].m_nodes[1] = g_elems[iID - 1].m_nodes[2];
							g_elems[iID - 1].m_nodes[2] = g_elems[iID - 1].m_nodes[4];
							g_elems[iID - 1].m_nodes[4] = g_elems[iID - 1].m_nodes[3];
							g_elems[iID - 1].m_nodes[3] = iTmp;
						}
						else if (strcmp(strCard, "E8Q") == 0) {
							g_elems[iID - 1].m_vtktype = 23;
							/* re-order the nodes into vtk standard */
							iTmp = g_elems[iID - 1].m_nodes[1];
							g_elems[iID - 1].m_nodes[1] = g_elems[iID - 1].m_nodes[2];
							g_elems[iID - 1].m_nodes[2] = g_elems[iID - 1].m_nodes[4];
							g_elems[iID - 1].m_nodes[4] = iTmp;
							iTmp = g_elems[iID - 1].m_nodes[3];
							g_elems[iID - 1].m_nodes[3] = g_elems[iID - 1].m_nodes[6];
							g_elems[iID - 1].m_nodes[6] = g_elems[iID - 1].m_nodes[5];
							g_elems[iID - 1].m_nodes[5] = iTmp;
						}
						else if (strcmp(strCard, "E4T") == 0 || (strcmp(strCard, "GE4") == 0 && g_dimension == 3))
							g_elems[iID - 1].m_vtktype = 10;
						else if (strcmp(strCard, "E5P") == 0)
							g_elems[iID - 1].m_vtktype = 14;
						else if (strcmp(strCard, "E6W") == 0 || strcmp(strCard, "GE6") == 0)
							g_elems[iID - 1].m_vtktype = 13;
						else if (strcmp(strCard, "E8H") == 0 || strcmp(strCard, "GE8") == 0)
							g_elems[iID - 1].m_vtktype = 12;
						else if (strcmp(strCard, "GE2") == 0)
							g_elems[iID - 1].m_vtktype = 3;					
					}
					else
						iErr += printf("* Element ID %d read on line %d is outside of expected range (1 to %d)\n",
									   iID, iLine, g_nelems);	
				}
				else
					iErr += printf("* Failed to read element information (ID, nodes, material) on line %d\n", iLine);					
			}
			/* read node */
			else if (strcmp(strCard, "ND") == 0 || strcmp(strCard, "GN") == 0) {
				if (sscanf(strLine, "%*s %d %le %le", &iID, &dX, &dY) == 3) {
					if (iID > 0 && iID <= g_nnodes) {
						++(g_nodes[iID - 1].m_nread);
						g_nodes[iID - 1].m_x = dX;
						g_nodes[iID - 1].m_y = dY;
						/* the Z coordinate must exist for 3D meshes */
						if (sscanf(strLine, "%*s %*d %*e %*e %le", &(g_nodes[iID - 1].m_z)) != 1 && g_dimension == 3)
							iErr += printf("* Failed to read node information (ID, coordinates) on line %d\n", iLine);
					}
					else
						iErr += printf("* Node ID %d read on line %d is outside of expected range (1 to %d)\n",
									   iID, iLine, g_nnodes);
				}
				else
					iErr += printf("* Failed to read node information (ID, coordinates) on line %d\n", iLine);
			}
			
		}
		else if (ferror(fpr)) {
			iErr += printf("* Error reading file line %d\n", iLine + 1);
			break;
		}
	}
	/* check IDs */
    for (ii = 0; ii < g_nelems; ++ii) {
        if (g_elems[ii].m_nread != 1)
            iErr += printf("* %s with ID %d\n", (g_elems[ii].m_nread == 0) ? "Missing element" : "Multiple elements",
						   ii + 1, a_filename);
	}
    for (ii = 0; ii < g_nnodes; ++ii) {
        if (g_nodes[ii].m_nread != 1)
            iErr += printf("* %s with ID %d\n", (g_nodes[ii].m_nread == 0) ? "Missing node" : "Multiple nodes",
						   ii + 1, a_filename);
	}
	/* clean up */
    fclose(fpr);
	/* exit early due to errors */
	if (iErr != 0) {
		printf("\n* Mesh file must be corrected before conversion can occur\n\nExiting\n\n");
		free(g_nodes);
		for (ii = 0; ii < g_nelems; ++ii)
			free(g_elems[ii].m_nodes);
		free(g_elems);		
		exit(0);
	}
	
    return;
}

/*! 
 \brief Read argument and categorize 
 */
static int get_arg_type(char *a_arg)
{
	make_lower_case(a_arg);
	if (strcmp(a_arg, "-f") == 0)
		return ARG_FORMAT;
	if (strcmp(a_arg, "-o") == 0)
		return ARG_OUTPUT;
	return ARG_UNKNOWN;
}

/*! 
 \brief Read file format and categorize 
 */
static int get_file_format_type(char *a_arg)
{
	make_lower_case(a_arg);
	if (strcmp(a_arg, "vtk") == 0)
		return FF_VTK;
	if (strcmp(a_arg, "vtu") == 0)
		return FF_VTU;
	/* default */
	return FF_VTU;
}

/*! 
 \brief Read dataset file line and categorize 
 */
static int get_dat_file_card_type(char *a_card)
{
	make_lower_case(a_card);
    /* Property Name */
    if (strcmp(a_card, "name") == 0)
        return DS_NAME;
    /* Timestep */
    if (strcmp(a_card, "ts") == 0)
        return DS_TS;
	/* Scalar data */
    if (strcmp(a_card, "begscl") == 0)
        return DS_BEGSCL;
	/* Vector data */
    if (strcmp(a_card, "begvec") == 0)
        return DS_BEGVEC;
	/* Nodes */
	if (strcmp(a_card, "nd") == 0)
		return DS_ND;
	/* Elements */
	if (strcmp(a_card, "nc") == 0)
		return DS_NC;
	/* Object Type */
	if (strcmp(a_card, "objtype") == 0)
		return DS_OBJTYPE;
    /* End of File */
    if (strcmp(a_card, "endds") == 0)
        return DS_ENDDS;
    return DS_UNKNOWN;
}

/*! 
   \brief Convert string to lowercase 
 */
void make_lower_case(char *a_str)
{
	int i = 0, len = strlen(a_str);

	for (i = 0; i < len; ++i)
		a_str[i] = tolower(a_str[i]);
}

/*! 
   \brief Trim leading and ending spaces in string
 */
void trim_spaces(char *a_str)
{
	int ii = 0, iLen = strlen(a_str);
	
	while (a_str[0] == ' ') {
		for (ii = 1; ii < iLen; ++ii)
			a_str[ii - 1] = a_str[ii];
		a_str[ii - 1] = '\0';
		--iLen;
	}
	
	ii = iLen - 1;
	while (a_str[ii] == ' ') {
		a_str[ii] = '\0';
		--ii;
	}
}

/*! 
   \brief Replace spaces in string to underscores
 */
void replace_spaces(char *a_str)
{
	int ii = 0, iLen = strlen(a_str);
			
	for (ii = 0; ii < iLen; ++ii)
		if (a_str[ii] == ' ')
			a_str[ii] = '_';
}

/*!
   \brief Reads dat file writes out vtk files
 */
void convert_dat_file(char *a_filebasename, char *a_datfilename, int a_firstdatfile)
{
	char strLine[STRLEN] = "", strCard[STRLEN] = "", strDatasetName[STRLEN] = "", strTmp[STRLEN] = "",
		 strOutputFileName[STRLEN] = "";
	double dTS = 0.0, *dData = NULL;
	int ii = 0, jj = 0, iLine = 0, iVelData = 0, iTmp = 0, *iData = NULL,
		iReadND = 0, iReadNC = 0, iReadOBJTYPE = 0, iReadBEG = 0,
		iNumDatasetsRead = 0, iNumTimestepsRead = 0;
    FILE *fpr = NULL, *fpw = NULL;
	
    /* Open File */
    fpr = fopen(a_datfilename, "r");	
    if (fpr == NULL) {
        printf("* Error reading file: %s\n\nExiting\n\n", a_datfilename);
        exit(0);
    }
    printf("Reading dataset file: %s\n\n", a_datfilename);

    /* read file */
    while (!feof(fpr)) {
		if (fgets(strLine, STRLEN, fpr)) {
			++iLine;
			if (sscanf(strLine, "%s", strCard) != 1) {
				printf("* Failed to read file card on line %d, file will be ignored\n\n", iLine);
				fclose(fpr);
				return;
			}
			switch (get_dat_file_card_type(strCard)) {
				case DS_NAME: /* Property Name */
					if (sscanf(strLine, "%*s %*[\"]%[^\"\r\n]", strDatasetName) == 1 || sscanf(strLine, "%*s %[^\r\n]", strDatasetName) == 1) {
						trim_spaces(strDatasetName);
						if (g_fileformat == FF_VTK) {
							replace_spaces(strDatasetName);
						}
					}
					else {
						printf("* Failed to read dataset name on line %d, name will be defaulted\n", iLine);
					}
					break;
				case DS_TS: /* Timestep */
					/* check that we have already read required cards */
					if (iReadND == 0 || iReadNC == 0 || iReadOBJTYPE == 0 || iReadBEG == 0) {
						printf("* Failed to read required dataset information (OBJTYPE, BEGSCL or BEGVEC, ND, NC), %s will be ignored\n\n",
							a_datfilename);
						fclose(fpr);
						return;
					}
					/* For each timestep a new vtk file will be written containing the mesh and the user specified data */
					if (sscanf(strLine, "%*s %d %le\n", &iTmp, &dTS) != 2) {
						printf("* Failed to read dataset timestep information on line %d, file will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}

					/* open output file for appending */
					sprintf(strOutputFileName,"%s.%d", a_filebasename, iNumTimestepsRead);
					++iNumTimestepsRead;
					if (iNumTimestepsRead > g_nTimesteps)
						g_nTimesteps = iNumTimestepsRead;
					/* if this is the first user specified dataset then the mesh must be written */
					if (a_firstdatfile) {
						sprintf(strTmp, "- Timestep: %lf", dTS);
						switch (g_fileformat) {
							case FF_VTK:
								write_vtk_file(strOutputFileName, strTmp);
								break;
							case FF_VTU:
							default:
								write_vtu_file_start(strOutputFileName, strTmp);								
								break;
						}							
					}
					else {
						switch (g_fileformat) {
							case FF_VTK:
								strcat(strOutputFileName, ".vtk");
								break;
							case FF_VTU:
							default:
								strcat(strOutputFileName, ".vtu");								
								break;
						}
					}
					fpw = fopen(strOutputFileName, "a");
					if (fpw == NULL) {
						printf("* Error writing file: %s\n\n", strOutputFileName);
						fclose(fpr);
						return;
					}

					/* activity array */
					if (iTmp == 1) {
						iData = (int *)malloc(sizeof(int) * g_nnodes);
						for (ii = 0; ii < g_nnodes; ++ii) {
							if (fgets(strLine, STRLEN, fpr)) {
								++iLine;
								if (sscanf(strLine, "%d", &iData[ii]) != 1) {
									printf("* Failed to read activity array data on line %d, file will be ignored\n\n", iLine);
									fclose(fpr);
									fclose(fpw);
									return;
								}
							}
							else if (ferror(fpr)) {
								printf("* Error reading file line %d, file will be ignored\n\n", iLine + 1);
								fclose(fpr);
								fclose(fpw);
								return;
							}
						}
						/* write to output file */
						switch (g_fileformat) {
							case FF_VTK:
								sprintf(strTmp, "%s_activity_array", strDatasetName);
								fprintf(fpw, "SCALARS %s int 1\n", strTmp);
								fprintf(fpw, "LOOKUP_TABLE default\n");
								for (ii = 0; ii < g_nnodes; ++ii)
									fprintf(fpw, "%d\n", iData[ii]);
								fprintf(fpw, "\n");
								break;
							case FF_VTU:
							default:
								fprintf(fpw, "<DataArray type=\"UInt8\" Name=\"%s activity array\" NumberOfComponents=\"1\" "
									"format=\"ascii\">\n", strDatasetName);
								for (ii = 0; ii < g_nnodes; ++ii)
									fprintf(fpw, "%d\n", iData[ii]);
								fprintf(fpw, "</DataArray>\n");						
								break;
						}
						/* clean up */
						free(iData);
						iData = NULL;
					}

					/* vector or scalar */
					if (iVelData) {
						/* vector data */
						dData = (double *)malloc(sizeof(double) * g_nnodes * 3);
						for (ii = 0, jj = 0; ii < g_nnodes; ++ii, jj += 3) {
							if (fgets(strLine, STRLEN, fpr)) {
								++iLine;
								if (sscanf(strLine, "%le %le", &dData[jj], &dData[jj + 1]) == 2) {
									/* the Z vector must exist for 3D meshes */
									dData[jj + 2] = 0.0;
									if (sscanf(strLine, "%*e %*e %le", &dData[jj + 2]) != 1 && g_dimension == 3) {
										printf("* Failed to read vector data on line %d, file will be ignored\n\n", iLine);
										fclose(fpr);
										fclose(fpw);
										return;
									}
								}
								else {
									printf("* Failed to read vector data on line %d, file will be ignored\n\n", iLine);
									fclose(fpr);
									fclose(fpw);
									return;
								}
							}
							else if (ferror(fpr)) {
								printf("* Error reading file line %d, file will be ignored\n\n", iLine + 1);
								fclose(fpr);
								fclose(fpw);
								return;
							}
						}
						/* write to output file */
						switch (g_fileformat) {
							case FF_VTK:
								fprintf(fpw, "VECTORS %s double\n", strDatasetName);
								for (ii = 0, jj = 0; ii < g_nnodes; ++ii, jj += 3)
									fprintf(fpw, "%16.15le %16.15le %16.15le\n", dData[jj], dData[jj + 1], dData[jj + 2]);
								fprintf(fpw, "\n");
								break;
							case FF_VTU:
							default:
								fprintf(fpw, "<DataArray type=\"Float64\" Name=\"%s\" NumberOfComponents=\"3\" format=\"ascii\">\n",
										strDatasetName);
								for (ii = 0, jj = 0; ii < g_nnodes; ++ii, jj += 3)
									fprintf(fpw, "%16.15le %16.15le %16.15le\n", dData[jj], dData[jj + 1], dData[jj + 2]);
								fprintf(fpw, "</DataArray>\n");					
								break;
						}
					}
					else {
						/* scalar */
						dData = (double *)malloc(sizeof(double) * g_nnodes);
						for (ii = 0; ii < g_nnodes; ++ii) {
							if (fgets(strLine, STRLEN, fpr)) {
								++iLine;
								if (sscanf(strLine, "%le", &dData[ii]) != 1) {
									printf("* Failed to read scalar data on line %d, file will be ignored\n\n", iLine);
									fclose(fpr);
									fclose(fpw);
									return;
								}
							}
							else if (ferror(fpr)) {
								printf("* Error reading file line %d, file will be ignored\n\n", iLine + 1);
								fclose(fpr);
								fclose(fpw);
								return;
							}
						}
						/* write to output file */
						switch (g_fileformat) {
							case FF_VTK:
								fprintf(fpw, "SCALARS %s double 1\n", strDatasetName);
								fprintf(fpw, "LOOKUP_TABLE default\n");
								for (ii = 0; ii < g_nnodes; ++ii)
									fprintf(fpw, "%16.15le\n", dData[ii]);
								fprintf(fpw, "\n");
								break;
							case FF_VTU:
							default:
								fprintf(fpw, "<DataArray type=\"Float64\" Name=\"%s\" NumberOfComponents=\"1\" format=\"ascii\">\n",
										strDatasetName);
								for (ii = 0; ii < g_nnodes; ++ii)
									fprintf(fpw, "%16.15le\n", dData[ii]);
								fprintf(fpw, "</DataArray>\n");						
								break;
						}
					}
					/* Close the output file and clean up the data */
					fclose(fpw); 
					free(dData);
					dData = NULL;
					/* inform user */					
					printf("Dataset \"%s\" - timestep %lf was written to %s\n", strDatasetName, dTS, strOutputFileName);	
					break;
				case DS_BEGSCL: /* Scalar data */
					iVelData = 0;
					iReadBEG = 1;
					++iNumDatasetsRead;
					sprintf(strDatasetName, "Dataset%d", iNumDatasetsRead);
					break;
				case DS_BEGVEC: /* Vector data */
					iVelData = 1;
					iReadBEG = 1;
					++iNumDatasetsRead;
					sprintf(strDatasetName, "Dataset%d", iNumDatasetsRead);
					break;
				case DS_ND: /* Node */
					iReadND = 1;
					if (sscanf(strLine, "%*s %d", &iTmp) != 1) {						
						printf("* Failed to read node information on line %d, file will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					if (iTmp != g_nnodes) {
						printf("* Dataset does not match mesh geometry (# of nodes on line %d) and will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					break;
				case DS_NC: /* Elements */
					iReadNC = 1;
					if (sscanf(strLine, "%*s %d", &iTmp) != 1) {						
						printf("* Failed to read element information on line %d, file will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					if (iTmp != g_nelems) {
						printf("* Dataset does not match mesh geometry (# of elements on line %d) and will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					break;
				case DS_OBJTYPE: /* Object Type */					
					iReadOBJTYPE = 1;
					if (sscanf(strLine, "%*s %s", strTmp) != 1) {
						printf("* Failed to read object type information on line %d, file will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					make_lower_case(strTmp);
					if (!strstr(strTmp, "mesh")) {
						printf("* Dataset does not match mesh geometry (object type on line %d) and will be ignored\n\n", iLine);
						fclose(fpr);
						return;
					}
					break;
				case DS_ENDDS:
					/* reset flags; OBJTYPE only needs to be read once and should not be reset*/
					a_firstdatfile = iReadBEG = iReadND = iReadNC = iNumTimestepsRead = 0;
					break;
				default:
					/* ignore */
					break;
			}
		}
		else if (ferror(fpr)) {
			printf("* Error reading file line %d, file will be ignored\n\n", iLine + 1);
			break;
		}
    }
	
	/* clean up */
    fclose(fpr);
	
    return;
}
