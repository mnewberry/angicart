for N in 1 2 3 ; do 
  DICOM_DIR=dicom_$N
  DICOM_SMALL_DIR=dicom_small_$N
  mkdir $DICOM_SMALL_DIR
  for FI in $DICOM_DIR/*[02468].png ; do
    N=`echo \`basename $FI\` | sed -e 's/.png*//g'`
    SRC1="$DICOM_DIR/$N.png"
    SRC2="$DICOM_DIR/`printf "%05d" \`dc -e"$N 1 + p"\``.png"
    DEST="$DICOM_SMALL_DIR/`printf "%05d" \`dc -e"$N 2 / p"\``.png"
    echo "$SRC1 $SRC2 -> $DEST"
    composite -blend 50% $SRC1 $SRC2 $DEST
    mogrify -shave 2x2 $DEST
    mogrify -scale 50% $DEST
  done
done
