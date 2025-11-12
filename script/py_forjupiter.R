# import http.client
# import json
#
# # Configura i parametri
# host = "geomatch-api-svc:8000" #pubblico https://ameliadpcoll.grins.it:61118 (sarà rimosso)
# path = "/invoke-geomatching"
#
# # Crea la connessione
# conn = http.client.HTTPConnection(host)
#
#
# body_dict= #aggiungere json
#
# json_data = json.dumps(body_dict)
# print("Body inviato:")
# print(json_data)
# headers = {'Content-type': 'application/json'}
# conn.request("POST", path, json_data, headers)
# response = conn.getresponse()
# response_data = response.read().decode('utf-8')
# print(f"Status: {response.status} {response.reason}")
# print(response_data)
# conn.close()


# classi delle colonne di output del dataset:
# "TINYINT",
# "SMALLINT",
# "INT",
# "BIGINT",
# "FLOAT",
# "DOUBLE",
# "DECIMAL",
# "BOOLEAN",
# "STRING",
# "VARCHAR",
# "CHAR",
# "BINARY",
# "TIMESTAMP",
# "DATE"

# STRING <- character
# DOUBLE <- numeric
# DATE <- Date

