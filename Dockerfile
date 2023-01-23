FROM mcr.microsoft.com/dotnet/core/sdk:2.2 as build

# Add keys and sources lists
RUN curl -sL https://deb.nodesource.com/setup_11.x | bash
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" \
    | tee /etc/apt/sources.list.d/yarn.list

# Install node, 7zip, yarn, git, process tools
RUN apt-get update && apt-get install -y nodejs p7zip-full git procps

WORKDIR /workspace

COPY . .

# Install fake
RUN dotnet tool install fake-cli -g --version 5.20.3

# Install Paket
RUN dotnet tool install paket -g --version 5.257.0

# add dotnet tools to path to pick up fake and paket installation
ENV PATH="/root/.dotnet/tools:${PATH}"

RUN npm install
RUN paket restore
RUN mkdir -p ./deploy/Client/public
RUN mkdir -p ./deploy/Server

RUN dotnet restore ./src/Client
RUN npm run build
RUN cp -r src/Client/deploy/* ./deploy/Client/public

RUN dotnet build src/Server/Server.fsproj
RUN dotnet publish --self-contained --runtime linux-x64 --configuration x64 -o ../../deploy/Server src/Server/Server.fsproj


# Second build step
FROM mcr.microsoft.com/dotnet/core/sdk:2.2

COPY --from=build /workspace/deploy /app
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

RUN ./configure --prefix=/usr/local/mpi 
RUN make -j all
RUN make install

ENV PATH=/usr/local/mpi/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/local/mpi/lib:$LD_LIBRARY_PATH

# Add cntk libs
WORKDIR /usr/local

RUN wget https://cntk.azurewebsites.net/BinaryDrop/CNTK-2-7-Linux-64bit-CPU-Only.tar.gz && \
    tar -xzf CNTK-2-7-Linux-64bit-CPU-Only.tar.gz && \
    rm -f CNTK-2-7-Linux-64bit-CPU-Only.tar.gz

RUN cp ./cntk/cntk/lib/Cntk.Core.CSBinding-2.7.so ./cntk/cntk/lib/libCntk.Core.CSBinding-2.7.dll

ENV PATH="/usr/local/cntk/cntk/lib:${PATH}"
ENV PATH="/usr/local/cntk/cntk/dependencies/lib:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/lib:${LD_LIBRARY_PATH}"
ENV LD_LIBRARY_PATH="/usr/local/cntk/cntk/dependencies/lib:${LD_LIBRARY_PATH}"

WORKDIR /app
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server/Server.dll" ]