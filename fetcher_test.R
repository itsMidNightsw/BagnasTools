## test fetcher for api data

#install.packages("httr") # if not intalled
install.packages("xml2")

library(httr)
library(jsonlite)
library(xml2)



headers = c(
  Authorization = "Bearer <your access token>"
)

files = list(
  request = '{\n    "input": {\n        "bounds": {\n            "properties": {\n                "crs": "http://www.opengis.net/def/crs/OGC/1.3/CRS84"\n            },\n            "bbox": [\n                13.822174072265625,\n                45.85080395917834,\n                14.55963134765625,\n                46.29191774991382\n            ]\n        },\n        "data": [\n            {\n                "type": "sentinel-2-l2a",\n                "dataFilter": {\n                    "timeRange": {\n                        "from": "2022-10-01T00:00:00Z",\n                        "to": "2022-10-31T00:00:00Z"\n                    }\n                }\n            }\n        ]\n    },\n    "output": {\n        "width": 512,\n        "height": 512\n    }\n}',
  evalscript = '//VERSION=3\n\nfunction setup() {\n  return {\n    input: ["B02", "B03", "B04"],\n    output: {\n      bands: 3,\n      sampleType: "AUTO" // default value - scales the output values from [0,1] to [0,255].\n    }\n  }\n}\n\nfunction evaluatePixel(sample) {\n  return [2.5 * sample.B04, 2.5 * sample.B03, 2.5 * sample.B02]\n}'
)

res <- httr::POST(url = "https://services.sentinel-hub.com/api/v1/process", httr::add_headers(.headers=headers), body = files, encode = "multipart")
