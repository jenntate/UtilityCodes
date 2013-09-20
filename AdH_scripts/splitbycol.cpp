#include<iostream>
#include<fstream>
#include<string>
#include<sstream>

using namespace std;

int main()
{
  int i,j,nnode,nelem,ncol=0;
  bool end=false;
  string name,line,temp,time,**val;
  string fname,num;
  stringstream ss;
  ifstream in;
  ofstream *out;

  cout<<"Input file: ";
  cin>>fname;

  in.open(fname.c_str());
  while(!in.is_open())
  {
    in.close();
    in.clear();
    cout<<fname<<" does not exist. Try again."<<endl;
    cout<<"Input file: ";
    cin>>fname;
    in.open(fname.c_str());
  }

  getline(in,line);
  getline(in,line);
  getline(in,line);
  in>>line>>nnode;
  in>>line>>nelem;
  in.ignore(1000,'\n');
  getline(in,line);
  ss<<line;
  ss>>temp;
  while(ss>>temp)
  {
    name+=temp;
    name+=" ";
  }
  ss.clear();
  name=name.substr(1,name.length()-3);
  in>>temp>>temp>>time;
  in.ignore(1000,'\n');
  getline(in,line);
  ss<<line;
  while(ss>>temp) ncol++; //just getting allocation int for *out
  ss.clear();

  val=new string*[nnode];
  for(i=0;i<nnode;i++) val[i]=new string[ncol];
  out=new ofstream[ncol];
  for(i=0;i<ncol;i++)
  {
    ss<<i+1;
    ss>>num;
    temp="col";
    temp+=num;
    temp+="_";
    temp+=fname;
    out[i].open(temp.c_str());
    out[i]<<"DATASET"<<endl;
    out[i]<<"OBJTYPE \"mesh2d\""<<endl;
    out[i]<<"BEGSCL"<<endl;
    out[i]<<"ND "<<nnode<<endl;
    out[i]<<"NC "<<nelem<<endl;
    out[i]<<"NAME \""<<name<<" Col"<<num<<"\""<<endl;
    ss.clear();
  }

  while(1)
  {
    for(i=0;i<nnode;i++)
    {
      ss<<line;
      for(j=0;j<ncol;j++) ss>>val[i][j];
      ss.clear();
      if(!(getline(in,line)))
      {
        end=true;
        break;
      }
      if(end) break;
    }
    if(end) break;
    ss<<line;
    if(!(getline(in,line))) break;
    else for(i=0;i<ncol;i++) out[i]<<"TS 0   "<<time<<endl;
    ss>>temp>>temp>>time;
    ss.clear();

    for(i=0;i<nnode;i++) for(j=0;j<ncol;j++) out[j]<<val[i][j]<<endl;
  }

  for(i=0;i<ncol;i++) out[i]<<"ENDDS"<<endl;

  return 0;
}
