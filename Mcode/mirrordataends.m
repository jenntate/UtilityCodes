function xm = mirrordataends(x,N,nf)
xm = zeros(2*nf+N,1);
xm(1:nf) = -(x(nf+1:-1:2)-x(1))+x(1);
xm(nf+1:N+nf) = x;
xm(N+nf+1:N+2*nf) = -(x(N-1:-1:N-nf)-x(N))+x(N);
return