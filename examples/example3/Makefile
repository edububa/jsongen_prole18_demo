javalib:
	(cd _build/default/lib; rm -rf json_schema_validator; git clone https://github.com/fge/json-schema-validator.git json_schema_validator; cd json_schema_validator; ./gradlew build)

compile:
	rebar3 compile
	make javalib

clean:
	rm -rf _build
	rm rebar.lock

test:
	rebar3 shell --name jsongen
