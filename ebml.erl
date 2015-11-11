-module(ebml).

-export([main/1]).


parse_vint(<<1:1, N:7, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:2, N:14, Rest/binary >>) ->
    {N, Rest};
parse_vint(<<1:3, N:21, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:4, N:28, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:5, N:35, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:6, N:42, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:7, N:49, Rest/binary>>) ->
    {N, Rest};
parse_vint(<<1:8, N:56, Rest/binary>>) ->
    {N, Rest}.

parse_subelements(_, <<>>, _) ->
    {[], <<>>};
parse_subelements(_, Bin, 0) ->
    {[], Bin};
parse_subelements(Offset, Bin, Size) ->
    {H, Bin1} = parse_element(Offset, Bin),
    {T, Bin2} = parse_subelements(Offset + byte_size(Bin) - byte_size(Bin1), Bin1, Size - byte_size(Bin) + byte_size(Bin1)),
    {[H|T], Bin2}.

parse_element_body(Offset, HeaderSize, Name, master, Size, Bin) ->
    {Elems, Bin1} = parse_subelements(Offset + HeaderSize, Bin, Size),
    {{Offset, Name, Size, Elems}, Bin1};
parse_element_body(Offset, _HeaderSize, Name, uinteger, Size, Bin) ->
    <<UInt:Size/unit:8, Bin2/binary>> = Bin,
    {{Offset, Name, Size, UInt}, Bin2};
parse_element_body(Offset, _HeaderSize, Name, integer, Size, Bin) ->
    <<Int:Size/signed-unit:8, Bin2/binary>> = Bin,
    {{Offset, Name, Size, Int}, Bin2};
parse_element_body(Offset, _HeaderSize, Name, float, Size, Bin) ->
    <<Float:Size/float-unit:8, Bin2/binary>> = Bin,
    {{Offset, Name, Size, Float}, Bin2};
parse_element_body(Offset, _HeaderSize, Name, string, Size, Bin) ->
    <<Str:Size/binary, Bin2/binary>> = Bin,
    {{Offset, Name, Size, Str}, Bin2};
parse_element_body(Offset, _HeaderSize, Name, 'utf-8', Size, Bin) ->
    <<Str:Size/binary, Bin2/binary>> = Bin,
    {{Offset, Name, Size, Str}, Bin2};
parse_element_body(Offset, _HeaderSize, Name, binary, Size, Bin) ->
    <<_:Size/binary, Bin2/binary>> = Bin,
    {{Offset, Name, Size, bin}, Bin2};
parse_element_body(Offset, _HeaderSize, ID, unknown, Size, Bin) ->
    <<_:Size/binary, Bin2/binary>> = Bin,
    {{Offset, ID, Size, unknown}, Bin2}.


element_type(16#A45DFA3) -> {'EBML', 'master'};
element_type(16#286) -> {'EBMLVersion', 'uinteger'};
element_type(16#2F7) -> {'EBMLReadVersion', 'uinteger'};
element_type(16#2F2) -> {'EBMLMaxIDLength', 'uinteger'};
element_type(16#2F3) -> {'EBMLMaxSizeLength', 'uinteger'};
element_type(16#282) -> {'DocType', 'string'};
element_type(16#287) -> {'DocTypeVersion', 'uinteger'};
element_type(16#285) -> {'DocTypeReadVersion', 'uinteger'};
element_type(16#6C) -> {'Void', 'binary'};
element_type(16#3F) -> {'CRC-32', 'binary'};
element_type(16#B538667) -> {'SignatureSlot', 'master'};
element_type(16#3E8A) -> {'SignatureAlgo', 'uinteger'};
element_type(16#3E9A) -> {'SignatureHash', 'uinteger'};
element_type(16#3EA5) -> {'SignaturePublicKey', 'binary'};
element_type(16#3EB5) -> {'Signature', 'binary'};
element_type(16#3E5B) -> {'SignatureElements', 'master'};
element_type(16#3E7B) -> {'SignatureElementList', 'master'};
element_type(16#2532) -> {'SignedElement', 'binary'};
element_type(16#8538067) -> {'Segment', 'master'};
element_type(16#14D9B74) -> {'SeekHeader', 'master'};
element_type(16#DBB) -> {'SeekPoint', 'master'};
element_type(16#13AB) -> {'SeekID', 'binary'};
element_type(16#13AC) -> {'SeekPosition', 'uinteger'};
element_type(16#549A966) -> {'Info', 'master'};
element_type(16#33A4) -> {'SegmentUID', 'binary'};
element_type(16#3384) -> {'SegmentFilename', 'utf-8'};
element_type(16#1CB923) -> {'PrevUID', 'binary'};
element_type(16#1C83AB) -> {'PrevFilename', 'utf-8'};
element_type(16#1EB923) -> {'NextUID', 'binary'};
element_type(16#1E83BB) -> {'NextFilename', 'utf-8'};
element_type(16#444) -> {'SegmentFamily', 'binary'};
element_type(16#2924) -> {'ChapterTranslate', 'master'};
element_type(16#29FC) -> {'ChapterTranslateEditionUID', 'uinteger'};
element_type(16#29BF) -> {'ChapterTranslateCodec', 'uinteger'};
element_type(16#29A5) -> {'ChapterTranslateID', 'binary'};
element_type(16#AD7B1) -> {'TimecodeScale', 'uinteger'};
element_type(16#AD7B2) -> {'TimecodeScaleDenominator', 'uinteger'};
element_type(16#489) -> {'Duration', 'float'};
element_type(16#461) -> {'DateUTC', 'date'};
element_type(16#3BA9) -> {'Title', 'utf-8'};
element_type(16#D80) -> {'MuxingApp', 'utf-8'};
element_type(16#1741) -> {'WritingApp', 'utf-8'};
element_type(16#F43B675) -> {'Cluster', 'master'};
element_type(16#67) -> {'ClusterTimecode', 'uinteger'};
element_type(16#1854) -> {'ClusterSilentTracks', 'master'};
element_type(16#18D7) -> {'ClusterSilentTrackNumber', 'uinteger'};
element_type(16#27) -> {'ClusterPosition', 'uinteger'};
element_type(16#2B) -> {'ClusterPrevSize', 'uinteger'};
element_type(16#23) -> {'SimpleBlock', 'binary'};
element_type(16#20) -> {'BlockGroup', 'master'};
element_type(16#21) -> {'Block', 'binary'};
element_type(16#22) -> {'BlockVirtual', 'binary'};
element_type(16#35A1) -> {'BlockAdditions', 'master'};
element_type(16#26) -> {'BlockMore', 'master'};
element_type(16#6E) -> {'BlockAddID', 'uinteger'};
element_type(16#25) -> {'BlockAdditional', 'binary'};
element_type(16#1B) -> {'BlockDuration', 'uinteger'};
element_type(16#7A) -> {'FlagReferenced', 'uinteger'};
element_type(16#7B) -> {'ReferenceBlock', 'integer'};
element_type(16#7D) -> {'ReferenceVirtual', 'integer'};
element_type(16#24) -> {'CodecState', 'binary'};
element_type(16#35A2) -> {'DiscardPadding', 'integer'};
element_type(16#E) -> {'Slices', 'master'};
element_type(16#68) -> {'TimeSlice', 'master'};
element_type(16#4C) -> {'SliceLaceNumber', 'uinteger'};
element_type(16#4D) -> {'SliceFrameNumber', 'uinteger'};
element_type(16#4B) -> {'SliceBlockAddID', 'uinteger'};
element_type(16#4E) -> {'SliceDelay', 'uinteger'};
element_type(16#4F) -> {'SliceDuration', 'uinteger'};
element_type(16#48) -> {'ReferenceFrame', 'master'};
element_type(16#49) -> {'ReferenceOffset', 'uinteger'};
element_type(16#4A) -> {'ReferenceTimeCode', 'uinteger'};
element_type(16#2F) -> {'EncryptedBlock', 'binary'};
element_type(16#654AE6B) -> {'Tracks', 'master'};
element_type(16#2E) -> {'TrackEntry', 'master'};
element_type(16#57) -> {'TrackNumber', 'uinteger'};
element_type(16#33C5) -> {'TrackUID', 'uinteger'};
element_type(16#3) -> {'TrackType', 'uinteger'};
element_type(16#39) -> {'TrackFlagEnabled', 'uinteger'};
element_type(16#8) -> {'TrackFlagDefault', 'uinteger'};
element_type(16#15AA) -> {'TrackFlagForced', 'uinteger'};
element_type(16#1C) -> {'TrackFlagLacing', 'uinteger'};
element_type(16#2DE7) -> {'TrackMinCache', 'uinteger'};
element_type(16#2DF8) -> {'TrackMaxCache', 'uinteger'};
element_type(16#3E383) -> {'TrackDefaultDuration', 'uinteger'};
element_type(16#34E7A) -> {'TrackDefaultDecodedFieldDuration', 'uinteger'};
element_type(16#3314F) -> {'TrackTimecodeScale', 'float'};
element_type(16#137F) -> {'TrackOffset', 'integer'};
element_type(16#15EE) -> {'MaxBlockAdditionID', 'uinteger'};
element_type(16#136E) -> {'TrackName', 'utf-8'};
element_type(16#2B59C) -> {'TrackLanguage', 'string'};
element_type(16#6) -> {'CodecID', 'string'};
element_type(16#23A2) -> {'CodecPrivate', 'binary'};
element_type(16#58688) -> {'CodecName', 'utf-8'};
element_type(16#3446) -> {'TrackAttachmentLink', 'uinteger'};
element_type(16#1A9697) -> {'CodecSettings', 'utf-8'};
element_type(16#1B4040) -> {'CodecInfoURL', 'string'};
element_type(16#6B240) -> {'CodecDownloadURL', 'string'};
element_type(16#2A) -> {'CodecDecodeAll', 'uinteger'};
element_type(16#2FAB) -> {'TrackOverlay', 'uinteger'};
element_type(16#16AA) -> {'CodecDelay', 'uinteger'};
element_type(16#16BB) -> {'SeekPreRoll', 'uinteger'};
element_type(16#2624) -> {'TrackTranslate', 'master'};
element_type(16#26FC) -> {'TrackTranslateEditionUID', 'uinteger'};
element_type(16#26BF) -> {'TrackTranslateCodec', 'uinteger'};
element_type(16#26A5) -> {'TrackTranslateTrackID', 'binary'};
element_type(16#60) -> {'TrackVideo', 'master'};
element_type(16#1A) -> {'VideoFlagInterlaced', 'uinteger'};
element_type(16#13B8) -> {'VideoStereoMode', 'uinteger'};
element_type(16#13B9) -> {'OldStereoMode', 'uinteger'};
element_type(16#30) -> {'VideoPixelWidth', 'uinteger'};
element_type(16#3A) -> {'VideoPixelHeight', 'uinteger'};
element_type(16#14AA) -> {'VideoPixelCropBottom', 'uinteger'};
element_type(16#14BB) -> {'VideoPixelCropTop', 'uinteger'};
element_type(16#14CC) -> {'VideoPixelCropLeft', 'uinteger'};
element_type(16#14DD) -> {'VideoPixelCropRight', 'uinteger'};
element_type(16#14B0) -> {'VideoDisplayWidth', 'uinteger'};
element_type(16#14BA) -> {'VideoDisplayHeight', 'uinteger'};
element_type(16#14B2) -> {'VideoDisplayUnit', 'uinteger'};
element_type(16#14B3) -> {'VideoAspectRatio', 'uinteger'};
element_type(16#EB524) -> {'VideoColourSpace', 'binary'};
element_type(16#FB523) -> {'VideoGamma', 'float'};
element_type(16#383E3) -> {'VideoFrameRate', 'float'};
element_type(16#61) -> {'TrackAudio', 'master'};
element_type(16#35) -> {'AudioSamplingFreq', 'float'};
element_type(16#38B5) -> {'AudioOutputSamplingFreq', 'float'};
element_type(16#1F) -> {'AudioChannels', 'uinteger'};
element_type(16#3D7B) -> {'AudioPosition', 'binary'};
element_type(16#2264) -> {'AudioBitDepth', 'uinteger'};
element_type(16#62) -> {'TrackOperation', 'master'};
element_type(16#63) -> {'TrackCombinePlanes', 'master'};
element_type(16#64) -> {'TrackPlane', 'master'};
element_type(16#65) -> {'TrackPlaneUID', 'uinteger'};
element_type(16#66) -> {'TrackPlaneType', 'uinteger'};
element_type(16#69) -> {'TrackJoinBlocks', 'master'};
element_type(16#6D) -> {'TrackJoinUID', 'uinteger'};
element_type(16#40) -> {'TrickTrackUID', 'uinteger'};
element_type(16#41) -> {'TrickTrackSegmentUID', 'binary'};
element_type(16#46) -> {'TrickTrackFlag', 'uinteger'};
element_type(16#47) -> {'TrickMasterTrackUID', 'uinteger'};
element_type(16#44) -> {'TrickMasterTrackSegmentUID', 'binary'};
element_type(16#2D80) -> {'ContentEncodings', 'master'};
element_type(16#2240) -> {'ContentEncoding', 'master'};
element_type(16#1031) -> {'ContentEncodingOrder', 'uinteger'};
element_type(16#1032) -> {'ContentEncodingScope', 'uinteger'};
element_type(16#1033) -> {'ContentEncodingType', 'uinteger'};
element_type(16#1034) -> {'ContentCompression', 'master'};
element_type(16#254) -> {'ContentCompAlgo', 'uinteger'};
element_type(16#255) -> {'ContentCompSettings', 'binary'};
element_type(16#1035) -> {'ContentEncryption', 'master'};
element_type(16#7E1) -> {'ContentEncAlgo', 'uinteger'};
element_type(16#7E2) -> {'ContentEncKeyID', 'binary'};
element_type(16#7E3) -> {'ContentSignature', 'binary'};
element_type(16#7E4) -> {'ContentSigKeyID', 'binary'};
element_type(16#7E5) -> {'ContentSigAlgo', 'uinteger'};
element_type(16#7E6) -> {'ContentSigHashAlgo', 'uinteger'};
element_type(16#C53BB6B) -> {'Cues', 'master'};
element_type(16#3B) -> {'CuePoint', 'master'};
element_type(16#33) -> {'CueTime', 'uinteger'};
element_type(16#37) -> {'CueTrackPositions', 'master'};
element_type(16#77) -> {'CueTrack', 'uinteger'};
element_type(16#71) -> {'CueClusterPosition', 'uinteger'};
element_type(16#70) -> {'CueRelativePosition', 'uinteger'};
element_type(16#32) -> {'CueDuration', 'uinteger'};
element_type(16#1378) -> {'CueBlockNumber', 'uinteger'};
element_type(16#6A) -> {'CueCodecState', 'uinteger'};
element_type(16#5B) -> {'CueReference', 'master'};
element_type(16#16) -> {'CueRefTime', 'uinteger'};
element_type(16#17) -> {'CueRefCluster', 'uinteger'};
element_type(16#135F) -> {'CueRefNumber', 'uinteger'};
element_type(16#6B) -> {'CueRefCodecState', 'uinteger'};
element_type(16#941A469) -> {'Attachments', 'master'};
element_type(16#21A7) -> {'AttachedFile', 'master'};
element_type(16#67E) -> {'FileDescription', 'utf-8'};
element_type(16#66E) -> {'FileName', 'utf-8'};
element_type(16#660) -> {'FileMimeType', 'string'};
element_type(16#65C) -> {'FileData', 'binary'};
element_type(16#6AE) -> {'FileUID', 'uinteger'};
element_type(16#675) -> {'FileReferral', 'binary'};
element_type(16#661) -> {'FileUsedStartTime', 'uinteger'};
element_type(16#662) -> {'FileUsedEndTime', 'uinteger'};
element_type(16#43A770) -> {'Chapters', 'master'};
element_type(16#5B9) -> {'EditionEntry', 'master'};
element_type(16#5BC) -> {'EditionUID', 'uinteger'};
element_type(16#5BD) -> {'EditionFlagHidden', 'uinteger'};
element_type(16#5DB) -> {'EditionFlagDefault', 'uinteger'};
element_type(16#5DD) -> {'EditionFlagOrdered', 'uinteger'};
element_type(16#36) -> {'ChapterAtom', 'master'};
element_type(16#33C4) -> {'ChapterUID', 'uinteger'};
element_type(16#1654) -> {'ChapterStringUID', 'utf-8'};
element_type(16#11) -> {'ChapterTimeStart', 'uinteger'};
element_type(16#12) -> {'ChapterTimeEnd', 'uinteger'};
element_type(16#18) -> {'ChapterFlagHidden', 'uinteger'};
element_type(16#598) -> {'ChapterFlagEnabled', 'uinteger'};
element_type(16#2E67) -> {'ChapterSegmentUID', 'binary'};
element_type(16#2EBC) -> {'ChapterSegmentEditionUID', 'uinteger'};
element_type(16#23C3) -> {'ChapterPhysicalEquiv', 'uinteger'};
element_type(16#F) -> {'ChapterTrack', 'master'};
element_type(16#9) -> {'ChapterTrackNumber', 'uinteger'};
element_type(16#0) -> {'ChapterDisplay', 'master'};
element_type(16#5) -> {'ChapterString', 'utf-8'};
element_type(16#37C) -> {'ChapterLanguage', 'string'};
element_type(16#37E) -> {'ChapterCountry', 'string'};
element_type(16#2944) -> {'ChapterProcess', 'master'};
element_type(16#2955) -> {'ChapterProcessCodecID', 'uinteger'};
element_type(16#50D) -> {'ChapterProcessPrivate', 'binary'};
element_type(16#2911) -> {'ChapterProcessCommand', 'master'};
element_type(16#2922) -> {'ChapterProcessTime', 'uinteger'};
element_type(16#2933) -> {'ChapterProcessData', 'binary'};
element_type(16#254C367) -> {'Tags', 'master'};
element_type(16#3373) -> {'Tag', 'master'};
element_type(16#23C0) -> {'TagTargets', 'master'};
element_type(16#28CA) -> {'TagTargetTypeValue', 'uinteger'};
element_type(16#23CA) -> {'TagTargetType', 'string'};
element_type(16#23C5) -> {'TagTrackUID', 'uinteger'};
element_type(16#23C9) -> {'TagEditionUID', 'uinteger'};
element_type(16#23C4) -> {'TagChapterUID', 'uinteger'};
element_type(16#23C6) -> {'TagAttachmentUID', 'uinteger'};
element_type(16#27C8) -> {'TagSimple', 'master'};
element_type(16#5A3) -> {'TagName', 'utf-8'};
element_type(16#47A) -> {'TagLanguage', 'string'};
element_type(16#484) -> {'TagDefault', 'uinteger'};
element_type(16#487) -> {'TagString', 'utf-8'};
element_type(16#485) -> {'TagBinary', 'binary'};
element_type(ID) ->
    {ID, unknown}.

parse_element(Offset, Bin) ->
    {ID, Bin1} = parse_vint(Bin),
    {Size, Bin2} = parse_vint(Bin1),
    {Name, Type} = element_type(ID),
    parse_element_body(Offset, byte_size(Bin) - byte_size(Bin2), Name, Type, Size, Bin2).

parse_elements(_, <<>>) ->
    {[], <<>>};
parse_elements(Offset, Bin) ->
    {H, Bin1} = parse_element(Offset, Bin),
    {T, Bin2} = parse_elements(Offset + byte_size(Bin) - byte_size(Bin1), Bin1),
    {[H|T], Bin2}.


main([Filename]) ->
    {ok, Bin} = file:read_file(Filename),
    io:format("~p~n", [parse_elements(0, Bin)]),
    ok.
