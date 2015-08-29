####[OpenWeatherMap][1] city id search service
=====================

####dependencies:
```
erlang-otp
rebar
ruby, ruby-dev
gem fpm
```

####default config
```
port=1488
```

####generate release
	make all
####launch release
	mare run
####generate deb-package
	make deb
	
###usage:
####search city by name
```
/api/search?name=samara
```
#####result:
```json
{
    "result": [
        {
            "id": "499099",
            "name": "Samara",
            "lat": "53.200001",
            "lon": "50.150002",
            "countryCode": "RU"
        },
        {
            "id": "3168599",
            "name": "Samarate",
            "lat": "45.625370",
            "lon": "8.783440",
            "countryCode": "IT"
        }
    ]
}
```
####short format:
```
/api/search?name=Samara&format=short
```
#####result:
```json
{
    "result": [
        {
            "id": "499099",
            "name": "Samara",
            "countryCode": "RU"
        },
        {
            "id": "3168599",
            "name": "Samarate",
            "countryCode": "IT"
        }
    ]
}
```
####2. codes of countries
```html
/api/codes
```
#####result:
```json
{
    "result": [
        "AD",
        "AE",
        "AG"
    ]
}
```

  [1]: http://openweathermap.com/
