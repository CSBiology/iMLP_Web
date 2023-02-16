# TargetP service iMLP

SAFE application serving a webservice that runs [iMLP](https://github.com/CSBiology/iMLP) via ASP.Net core

## Run locally

#### Follow all prerequisites laid out [here](https://github.com/CSBiology/iMLP#general) to make the backend work

You need .NET 6 + runtime/SDK

- `dotnet tool restore`
- `dotnet run` -> will open a browser and locally serve the application

## Docker Deploy

- change `DeployMode` to `Docker` (in Server.fs)
- increase SemVer version in package.json.
- `dotnet run bundle` (don't ask why. you need this. no it does not make too much sense.)
- `docker build -t imlp-web .` (Will take some time because of dependency download)
- `docker run -it -p 5000:5000 imlp-web` (for testing)
- `docker tag imlp-web:latest csbdocker/imlp-web:latest`
- `docker tag imlp-web:latest csbdocker/imlp-web:<New_Version>`
- login into csbdocker docker account (`docker login`)
- `docker push csbdocker/imlp-web:latest`
- `docker push csbdocker/imlp-web:<New_Version>`