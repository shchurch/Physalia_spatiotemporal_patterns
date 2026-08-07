ID=$1
DIR="${ID}_results_files"

mkdir $DIR
cp getorganelle/${ID}_mtgenome/*path*fasta  $DIR/
cp mitos_results/${ID}/result.bed  $DIR/
cp tRNAscan_results/${ID}/*result*txt  $DIR/

zip -r $DIR.zip $DIR
rm -r $DIR
