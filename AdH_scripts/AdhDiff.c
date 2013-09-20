#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {

FILE *fp1, *fp2;
double float1, float2, rdum, error, max_error, avg_error;
char string1[25], string2[25]; 
int k, idum, strcmp1=1, strcmp2=1;

fp1 = fopen(argv[1], "r" );
fp2 = fopen(argv[2], "r" );

if (!fp1) {
	printf("Unable to open file: %s \n",argv[1]);
	return 0;
}
if (!fp2) {
    printf("Unable to open file: %s \n",argv[2]);
    return 0;
}

k=0;
avg_error = 0;
max_error = 0;
do {
	k++;

	fscanf(fp1,"%s",string1);
	fscanf(fp2,"%s",string2);

	strcmp1 = strncmp(string1,"TS",2);
	strcmp2 = strncmp(string2,"TS",2);
	if (strcmp1==0 || strcmp2==0) {
		fscanf(fp1,"%d %lf",&idum,&rdum);
		fscanf(fp2,"%d %lf",&idum,&rdum);
		printf("Time: %lfs\n",rdum);
		//printf("str1: %s \t str2: %s \t strcmp1: %d \t strcmp2: %d \n",string1, string2, strcmp1, strcmp2);
	}


	strcmp1 = strcmp(string1,"ENDDS");
	strcmp2 = strcmp(string2,"ENDDS");	
	if (strcmp1 == 0 || strcmp2 == 0 ) {
		if (strcmp1 != 0 || strcmp2 != 0 ) {
			printf("strcmp1: %d \t strcmp2: %d \n",strcmp(string1,"ENDDS"),strcmp(string2,"ENDDS"));
			printf("Files are not the same length :: line1: %s \t line2: %s \n",string1,string2);
			printf("EXITING.\n");
		}
		break;
	}
	sscanf (string1,"%lf",&float1);
	sscanf (string2,"%lf",&float2);

	error = fabs(float1 - float2);
	//printf("float1: %30.20e \t float2: %30.20e \t error %30.20e \n",float1, float2, error);
	avg_error += error;
	if (error > max_error) max_error = error;
	

	//printf("%d \t %20.15f \t %20.15f \n",k,float1,float2);
} while(1);

avg_error /= (double) k;

printf("Average Error in Files :: %30.20e \n",avg_error);
printf("Max Error in Files     :: %30.20e \n",max_error);

return 0;

}


