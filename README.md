# Draenor

Convert OSM PBF files into ORC, and upload them to S3.

### Dependencies ###

- [`stack`](https://docs.haskellstack.org/en/stable/README/)
- [aws CLI tools](https://aws.amazon.com/cli/) and valid AWS credentials
- [`osm2orc`](https://github.com/mojodna/osm2orc) (instructions below)
- Java 8
- `git`

### Building the Tool ###

First, we build `osm2orc`. Clone its repository:

```bash
git clone git@github.com:mojodna/osm2orc.git
```

and follow [these instructions](https://github.com/mojodna/osm2orc#build) to build
an executable.

Now clone and build `draenor`:

```
git clone git@github.com:fosskers/draenor.git
cd draenor
stack build
```

The `stack build` step may take a few minutes if you don't already have a Haskell
development environment set up.

### Usage ###

```
colin@yumi ~/c/h/draenor> stack exec -- draenor -h
.osm.pbf -> .orc, and upload to S3.

Usage: draenor --areas STRING --cache FILEPATH --osm2orc FILEPATH --s3 TEXT --threads INT

Available options:
  -h,--help                Show this help text
  --areas STRING           Path to a JSON file defining areas to convert.
  --cache FILEPATH         Path to a directory for storing the downloaded .pbf files.
  --osm2orc FILEPATH       Path to an osm2orc executable.
  --s3 TEXT                S3 Bucket/Key to upload to, like: s3://vectortiles/orc/
  --threads INT            Number of CPU threads to use.
```
