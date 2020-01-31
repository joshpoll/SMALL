from flask import Flask, request
from flask_restful import Resource, Api
import subprocess
import json
from flask_cors import CORS
from os import mkdir

app = Flask(__name__)
CORS(app)
api = Api(app)


class HelloWorld(Resource):
    def post(self):
        fields = request.get_json()
        file_name = fields['file_name']
        program = fields['program']
        with open(f"../../sml-files/{file_name}.sml", 'w+') as f:
            f.write(program)
        cmd = ["./hamlet/hamlet",
               "-p", f"../../sml-files/{file_name}.sml"]
        result = subprocess.run(cmd, capture_output=True)
        with open(f"../../sml-files/{file_name}.json", 'w+') as f:
            f.write(str(json.loads(result.stdout)))
        return json.loads(result.stdout)


api.add_resource(HelloWorld, '/')

if __name__ == '__main__':
    try:
        mkdir("../../sml-files/")
    except FileExistsError:
        pass
    app.run(debug=True)
