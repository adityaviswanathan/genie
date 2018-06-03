RACO_PATH="/Applications/Racket v6.12/bin/raco"
PROTOS_PATH="."
DEFS_PATH="../defs"
"$RACO_PATH" pkg install --skip-installed planet-murphy-protobuf1
protoc -I=$PROTOS_PATH --racket_out=$DEFS_PATH *.proto
