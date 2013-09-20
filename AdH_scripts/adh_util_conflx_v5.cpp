#include<iostream>
#include<fstream>
#include<iomanip>
#include<string>
#include<sstream>
#include<cmath>

using namespace std;

int main()
{
  int i,j;            //duh
  int icon;           //constituent id
  int nstring=0;      //number of strings
  int ncon;           //number of constituents
  int istr;           //current flux string
  int *str;           //list of the used strings
  double interval;    //user chosen sample interval (0=everything)
  double ttprint;     //next time to print
  double time=0.0;    //current time
  double ptime;       //previous time
  double dt;          //current dt
  double *totalmass;
  double **sflux;     //suspended sediment flux
  double **bflux;     //bedload flux
  double **totsflux;  //total suspended sediment flux
  double **totbflux;  //total bedload flux
  string input;       //input filename
  string output;      //output filename
  string junk;        //stores unused strings
  string junk2;
  ifstream in;        //input file
  ofstream *out;      //output file
  ofstream *outtot;   //output file
  stringstream ss;    //store the string ids
  stringstream stemp; //convert number to string

  cout<<"Input File Name: ";
  cin>>input;
  //input="test";

  in.open(input.c_str());
  while(!in.is_open())
  {
    in.close();
    in.clear();
    cout<<input<<" does not exist. Try again."<<endl;
    cout<<"Input File Name: ";
    cin>>input;
    in.open(input.c_str());
  }

  cout<<"Minimum Print Interval: ";
  cin>>interval;
  //interval=1;
  cout<<"Output File Name: ";
  cin>>output;
  //output="out";

  in>>junk>>time;

  do
  { //count the numbers of strings and constituents
    in>>junk>>dt>>junk;
    junk=junk.substr(7,junk.length()); //pop out the string id
    istr=atoi(junk.c_str());
    ss<<istr<<" ";

    in>>junk;

    do
    { //constituents
      in>>icon;

      in>>junk>>junk;
      in>>junk>>junk>>junk;

      in>>junk;
    } while(junk!="TIME:");

    ptime=time;
    in>>time;

    nstring++;
  } while(time==ptime);

  ncon=icon; //clean this up later

  in.seekg(0,std::ios::beg);  //REWIND i don't think this works

  //commence work!
  str=new int[nstring];
  sflux=new double*[nstring];
  bflux=new double*[nstring];
  totsflux=new double*[nstring];
  totbflux=new double*[nstring];
  totalmass=new double[nstring];
  out=new ofstream[nstring];
  outtot=new ofstream[nstring];

  for(i=0;i<nstring;i++)
  {
    sflux[i]=new double[ncon];
    bflux[i]=new double[ncon];
    totsflux[i]=new double[ncon];
    totbflux[i]=new double[ncon];

    ss>>str[i];

    stemp<<str[i];
    stemp>>junk2;
    junk=output+"_"+"str"+junk2;
    out[i].open(junk.c_str());
    out[i]<<"Time\tString\t";

    junk=output+"_total_"+"str"+junk2;
    outtot[i].open(junk.c_str());
    stemp.clear();
    outtot[i]<<"Time     TotalMass\t";

    totalmass[i]=0.0;

    for(j=0;j<ncon;j++)
    {
      sflux[i][j]=0.0;
      bflux[i][j]=0.0;
      totsflux[i][j]=0.0;
      totbflux[i][j]=0.0;

      out[i]<<"ConFlux"<<j+1<<"\t"<<"ConMass"<<j+1<<"\t"<<"BedFlux"<<j+1<<"\t"<<"BedMass"<<j+1<<"\t";
    }
    out[i]<<endl;
    outtot[i]<<endl;
  }

  time=ttprint=0.0;

  if(interval<=0)
  {
    in>>junk;
    while(!in.eof())
    {
      for(i=0;i<nstring;i++)
      {
        in>>time>>junk>>dt>>junk;

        out[i]<<time<<"\t"<<str[i];

        out[i]<<scientific<<setprecision(12);
        outtot[i]<<scientific<<setprecision(12);
        for(j=0;j<ncon;j++)
        { //constituents
          in>>junk>>icon;

          in>>junk>>sflux[i][j]>>junk>>junk>>bflux[i][j];

          totsflux[i][j]+=sflux[i][j] * dt;
          totbflux[i][j]+=bflux[i][j] * dt;
          totalmass[i]  +=sflux[i][j] * dt +
                          bflux[i][j] * dt;

          out[i]<<"\t"<<sflux[i][j]<<"\t"<<totsflux[i][j]<<"\t"<<bflux[i][j]<<"\t"<<totbflux[i][j];
        }
        outtot[i]<<time<<"\t"<<totalmass[i];

        out[i].unsetf(ios::scientific);
        outtot[i].unsetf(ios::scientific);

        out[i]<<endl;
        outtot[i]<<endl;
        in>>junk;
      }
    }
  }
  else
  {
    in>>junk;
    while(!in.eof())
    {
      for(i=0;i<nstring;i++)
      {
        in>>time;
        ttprint=time-ptime;
        in>>junk>>dt>>junk;

        if(ttprint>=interval || time==0.0)
        {
          out[i]<<time<<"\t"<<str[i];
        }

        out[i]<<scientific<<setprecision(12);
        outtot[i]<<scientific<<setprecision(12);
        for(j=0;j<ncon;j++)
        { //constituents
          in>>junk>>icon;

          in>>junk>>sflux[i][j]>>junk>>junk>>bflux[i][j];

          totsflux[i][j]+=sflux[i][j] * dt;
          totbflux[i][j]+=bflux[i][j] * dt;
          totalmass[i]  +=sflux[i][j] * dt +
                          bflux[i][j] * dt;

          if(ttprint>=interval || time==0.0)
            out[i]<<"\t"<<sflux[i][j]<<"\t"<<totsflux[i][j]<<"\t"<<bflux[i][j]<<"\t"<<totbflux[i][j];
        }

        if(ttprint>=interval || time==0.0)
        {
          outtot[i]<<time<<"\t"<<totalmass[i];
        }

        out[i].unsetf(ios::scientific);
        outtot[i].unsetf(ios::scientific);

        if(ttprint>=interval || time==0.0)
          out[i]<<endl;
        if(ttprint>=interval || time==0.0)
          outtot[i]<<endl;
        in>>junk;
      }

      if(ttprint>=interval)
      {
        ttprint=0.0;
        ptime=time;
      }
    }
  }

  return 0;
}
