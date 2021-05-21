# TargetP service iMLP

SAFE application serving a webservice that runs [iMLP](https://github.com/CSBiology/iMLP) via ASP.Net core

## Run locally

You need .NET 2.2. runtime/SDK and any modern SDK (3.1 , 5.0)

- `dotnet tool restore`
- `dotnet fake build -t run` -> will open a browser and locally serve the application

## Deploy (e.g. for IIS)

- `change deploy mode to server (in Server.fs)`
- `dotnet fake build -t bundle` -> generates a `publish` folder that can be used as web application e.g. for NGINX, IIS, etc.
