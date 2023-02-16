FROM mcr.microsoft.com/dotnet/sdk:6.0 as build
# Developer
LABEL author Kevin Frey <freymaurer@gmx.de>
LABEL author Kevin Schneider <schneike@bio.uni-kl.de>

# Install node
# node_18 will not work because some lazyView2 function from fable is not supported anymore. (see console)
RUN curl -sL https://deb.nodesource.com/setup_16.x | bash
RUN apt-get update && apt-get install -y nodejs

WORKDIR /workspace
COPY ../. .
RUN dotnet tool restore
RUN npm install
RUN dotnet fable src/Client -o src/Client/output -e .fs.js -s --run webpack --mode production
#RUN cd src/Server && dotnet publish --self-contained --runtime linux-x64 --configuration x64 -o ../../deploy
RUN cd src/Server && dotnet publish -c Release -o ../../deploy
# RUN dotnet publish --self-contained --runtime linux-x64 --configuration x64 -o ../../deploy/Server src/Server/Server.fsproj

# Second build step
FROM mcr.microsoft.com/dotnet/sdk:6.0 as dependencies

#COPY --from=build /workspace/deploy /app
WORKDIR /usr/local

RUN apt-get update -y \
    && apt-get install -y libnuma-dev \
    && apt-get install -y build-essential \
    && apt-get install -y g++

# Add openmpi libs
RUN wget https://www.open-mpi.org/software/ompi/v1.10/downloads/openmpi-1.10.3.tar.gz \
    && tar -xzvf openmpi-1.10.3.tar.gz \
    && rm -f openmpi-1.10.3.tar.gz

WORKDIR /usr/local/openmpi-1.10.3

#RUN ./configure --prefix=/usr/local/mpi 
#RUN make -j all
#RUN make install
#
#ENV PATH=/usr/local/mpi/bin:$PATH
#ENV LD_LIBRARY_PATH=/usr/local/mpi/lib:$LD_LIBRARY_PATH

# Add cntk libs
WORKDIR /usr/local

RUN wget https://cntk.azurewebsites.net/BinaryDrop/CNTK-2-7-Linux-64bit-CPU-Only.tar.gz && \
    tar -xzf CNTK-2-7-Linux-64bit-CPU-Only.tar.gz && \
    rm -f CNTK-2-7-Linux-64bit-CPU-Only.tar.gz

RUN cp ./cntk/cntk/lib/Cntk.Core.CSBinding-2.7.so ./cntk/cntk/lib/libCntk.Core.CSBinding-2.7.dll

#ENV PATH="/usr/local/cntk/cntk/lib:${PATH}"
#ENV PATH="/usr/local/cntk/cntk/dependencies/lib:${PATH}"
#ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/lib:${LD_LIBRARY_PATH}"
#ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/dependencies/lib:${LD_LIBRARY_PATH}"


# Trying to minimize image size
FROM mcr.microsoft.com/dotnet/sdk:6.0

COPY --from=build /workspace/deploy /app
COPY --from=dependencies /usr/local/openmpi-1.10.3 /usr/local/openmpi-1.10.3
COPY --from=dependencies /usr/local/cntk /usr/local/cntk

RUN apt-get update -y \
    && apt-get install -y g++ \
    && apt-get install make

WORKDIR /usr/local/openmpi-1.10.3

RUN ./configure --prefix=/usr/local/mpi 
RUN make -j all
RUN make install

ENV PATH=/usr/local/mpi/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/local/mpi/lib:$LD_LIBRARY_PATH

ENV PATH="/usr/local/cntk/cntk/lib:${PATH}"
ENV PATH="/usr/local/cntk/cntk/dependencies/lib:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/lib:${LD_LIBRARY_PATH}"
ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/dependencies/lib:${LD_LIBRARY_PATH}"

WORKDIR /app
EXPOSE 5000
ENTRYPOINT [ "dotnet", "Server.dll" ]