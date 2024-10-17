install.packages(c("shinylive", "httpuv"))

path = "/Users/binghui/Desktop/2024 Fall/Web Mapping/Shinylive"

shinylive::export(appdir = path, destdir = "docs")
