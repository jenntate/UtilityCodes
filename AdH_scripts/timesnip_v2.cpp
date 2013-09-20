/*  David S. Smith
  This shrinks a dataset file by doing one of three things:
    1) Extracts the data at every nth time step
    2) Extracts every nth average of the data
    3) Extracts a window of data from one time value to the next

  Input:
    Dataset filename
    Output filename
    Choice of extraction (1, 2, or 3)
    Number of columns 

  Input (Choice = 1 or 2)
    Interval at which to extract data (n)

  Input (Choice = 3)
    Start time
    End time
*/

#include<iostream>
#include<fstream>
#include<cmath>
#include<iomanip>
#include<string>
#include<sstream>

#define HEADERS 6

using namespace std;

int main()
{
  int i,j,ans,nnode,ncol=1;
  double **data,time,atime,n,dt;
  double a,b,**pdata,pdt,ptime;
  double **datadt,**pdatadt,sumdt,psumdt,nsteps;
  bool end=false;
  string name,line,type,card;
  ifstream in;
  ofstream out;

  cout<<"Dataset file name: ";
  cin>>name;

  in.open(name.c_str());
  while(!in.is_open())
  {
    in.clear();
    cout<<name<<" does not exist. Try again."<<endl;
    cout<<"Dataset file name: ";
    cin>>name;
    in.open(name.c_str());
  }

  cout<<"Output file name: ";
  cin>>name;
  out.open(name.c_str());

  cout<<"What method would you like to use?"<<endl;
  cout<<"1) Extract every x seconds(minutes/hours)"<<endl;
  cout<<"2) Extract an average every x seconds (minutes/hours) "<<endl;
  cout<<"3) Extract a window of time"<<endl;
  cout<<"Choice: ";
  cin>>ans;
  while(ans<1 || ans>3)
  {
    cout<<ans<<" is not a choice. Try again: ";
    cin>>ans;
  }

  if(ans==1 || ans==2)
  {
    cout<<"Desired x seconds (minutes/hours): ";
    cin>>n;
  }
  else if(ans==3)
  {
    cout<<"Extraction starting time: ";
    cin>>a;
    cout<<"Extraction ending time: ";
    cin>>b;

    while(a>b)
    {
      cout<<"Start time cannot be greater than end time. Try again."<<endl;
      cout<<"Extraction starting time: ";
      cin>>a;
      cout<<"Extraction ending time: ";
      cin>>b;
    }
  }
  else
  {
    cout<<"No, no, no. This isn't right at all."<<endl;
    exit(0);
  }

  getline(in,line);
  out<<line<<endl;
  getline(in,line);
  out<<line<<endl;
  in>>type>>card>>nnode>>card>>i;
  out<<type<<endl<<"ND "<<nnode<<endl<<"NC "<<i<<endl;
  in.ignore(1000,'\n');
  getline(in,line);
  out<<line<<endl;
  out<<scientific<<setprecision(8);
  cout<<"Number of columns of data in file.";
  cin>>ncol;
  data=new double*[ncol];
  pdata=new double*[ncol];
  datadt=new double*[ncol];
  pdatadt=new double*[ncol];
  for(i=0;i<ncol;i++)
  {
    data[i]=new double[nnode];
    pdata[i]=new double[nnode];
    datadt[i]=new double[nnode];
    pdatadt[i]=new double[nnode];
    for(j=0;j<nnode;j++)
    {
      pdata[i][j]=-999;
      pdatadt[i][j]=-999;
    }
  }

  if(ans==1)
  {
    if(!(in>>card>>i>>time)) exit(0);
    atime=ptime=time;
    
    while(1)
    {
      for(i=0;i<nnode;i++)
      {
        for(j=0;j<ncol;j++)
          if(!(in>>data[j][i]))
          {
            end=true;
            break;
          }
        if(end) break;
      }
      if(end) break;
      if(time>=atime+n || time==atime)
      {
        dt=fabs(time-(atime+n));
        pdt=fabs(ptime-(atime+n));
        if(dt<=pdt) out<<"TS 0 "<<time<<endl;
        else out<<"TS 0 "<<ptime<<endl;
        for(i=0;i<nnode;i++)
        {
          for(j=0;j<ncol;j++)
          {
            if(dt<=pdt) out<<" "<<data[j][i];
            else out<<" "<<pdata[j][i];
            pdata[j][i]=data[j][i];
          }
          out<<endl;
        }
      }
      if(time>=atime+n) atime+=n;
      ptime=time;
      if(!(in>>card>>i>>time)) break;
    }
  }
  else if(ans==2)
  {
    if(!(in>>card>>i>>time)) exit(0);
    atime=ptime=time;
    sumdt=psumdt=nsteps=0.0;
    //cout<<sumdt<<endl;
    for(i=0;i<nnode;i++)
      for(j=0;j<ncol;j++)
      {
        if(!(in>>data[j][i])) exit(0);
        else pdata[j][i]=data[j][i];
        datadt[j][i]=pdatadt[j][i]=0.0;
      }

    while(1)
    {
      if(!(in>>card>>i>>time)) break;
      psumdt=sumdt;
      dt=time-ptime;
      sumdt+=dt;
      //cout<<sumdt<<endl;
      for(i=0;i<nnode;i++)
      {
        for(j=0;j<ncol;j++)
          if(!(in>>data[j][i]))
          {
            end=true;
            break;
          }
          else
          {
            pdatadt[j][i]=datadt[j][i];
            //datadt[j][i]+=(dt/6.0)*(data[j][i]+2*(data[j][i]+pdata[j][i])+pdata[j][i]);
            datadt[j][i]+=0.5*dt*(data[j][i]+pdata[j][i]);
            pdata[j][i]=data[j][i];
          }
        if(end) break;
      }
      if(end) break;
      nsteps+=1.0;
      if(time>=atime+n)
      {
        dt=fabs(time-(atime+n));
        pdt=fabs(ptime-(atime+n));
        if(dt<=pdt) out<<"TS 0 "<<atime+sumdt/2.0<<endl;
        else out<<"TS 0 "<<atime+psumdt/2.0<<endl;
        for(i=0;i<nnode;i++)
        {
          for(j=0;j<ncol;j++)
          {
            if(dt<=pdt)
            {
              //out<<" "<<datadt[j][i]/nsteps;
              out<<" "<<datadt[j][i]/sumdt;
              datadt[j][i]=0.0;
            }
            else
            {
              //out<<" "<<pdatadt[j][i]/(nsteps-1.0);
              out<<" "<<pdatadt[j][i]/psumdt;
              datadt[j][i]=datadt[j][i]-pdatadt[j][i];
            }
            pdata[j][i]=data[j][i];
            pdatadt[j][i]=0.0;
          }
          out<<endl;
        }
        if(dt<=pdt) nsteps=0.0;
        else nsteps=1.0;
        atime+=n;
        sumdt=0.0;
      }
      ptime=time;
    }
  }
  else if(ans==3)
  {
    while(1)
    {
      in>>card>>i>>time;
      for(i=0;i<nnode;i++)
      {
        for(j=0;j<ncol;j++)
          if(!(in>>data[j][i]))
          {
            end=true;
            break;
          }
        if(end) break;
      }
      if(end) break;
      if(time>=a && time<=b)
      {
        out<<"TS 0 "<<time<<endl;
        for(i=0;i<nnode;i++)
        {
          for(j=0;j<ncol;j++) out<<" "<<data[j][i];
          out<<endl;
        }
      }
      else if(time>b)
        break;
    }
  }
  else
  {
    cout<<"Alright... How'd this happen?"<<endl;
    exit(0);
  }

  out<<"ENDDS"<<endl;

  return 0;
}
