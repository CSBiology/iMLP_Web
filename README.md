# TargetP service iMLP

SAFE application serving a webservice that runs [iMLP](https://github.com/CSBiology/iMLP) via ASP.Net core

## Run locally

You need .NET 2.2. runtime/SDK and any modern SDK (3.1 , 5.0)

- `dotnet tool restore`
- `dotnet fake build -t run` -> will open a browser and locally serve the application

## Deploy (e.g. for IIS)

- `change deploy mode to server (in Server.fs)`
- `dotnet fake build -t bundle` -> generates a `publish` folder that can be used as web application e.g. for NGINX, IIS, etc.

## Docker Deploy

- change `DeployMode` to `Docker` (in Server.fs)
- increase SemVer version in package.json.
- `docker build -t imlp-web .` (Will take some time because of dependency download)
- `docker run -it -p 8085:8085 imlp-web` (for testing)
- `docker tag imlp-web:latest csbdocker/imlp-web:latest`
- `docker tag imlp-web:latest csbdocker/imlp-web:<New_Version>`
- login into csbdocker docker account (`docker login`)
- `docker push csbdocker/imlp-web:latest`
- `docker push csbdocker/imlp-web:<New_Version>`