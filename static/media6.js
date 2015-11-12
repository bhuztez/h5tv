"use strict";

function get_vint_length(c) {
    var i;
    for (i=9; c>0; i--) {
        c = c >> 1;
    }
    return i;
}

function get_vint_value(array, offset, length) {
    var v = array[offset] - (1 << (8 - length));

    for (var i=1; i<length; i++) {
        v = (v << 8) + array[offset+i];
    }

    return v;
}

function parse_header_size(buffer) {
    var view = new DataView(buffer);
    var array = new Uint8Array(buffer);
    if (view.getUint32(0, false) !== 0x1a45dfa3) {
        throw "EBML Element ID not found";
    }

    if (array[4] === 0) {
        throw "Bad EBML Size";
    }

    var length = get_vint_length(array[4]);
    var ebml_size = get_vint_value(array, 4, length);
    var segment_offset = 4 + length + ebml_size;

    if (view.getUint32(segment_offset, false) !== 0x18538067) {
        throw "Segment Element ID not found";
    }

    var size_length = get_vint_length(array[segment_offset+4]);
    var offset = segment_offset + 4 + size_length;

    while (view.getUint32(offset, false) != 0x1F43B675) {
        offset += get_vint_length(array[offset]);
        var elem_length = get_vint_length(array[offset]);
        var elem_size = get_vint_value(array, offset, elem_length);
        offset += elem_length + elem_size;
    }

    return offset;
}

window.addEventListener(
    "load",
    function() {
        var videoElem = document.getElementById("video");
        var mediaSource = new MediaSource();
        var sourceBuffer;
        var buffers = [];
        var updating = false;
        var buffer_count = 0;
        var started_play = false;
        var skipped = false;
        videoElem.src = URL.createObjectURL(mediaSource);

        function notify() {
            if ((updating) || (buffers.length === 0)) {
                return;
            }
            updating = true;
            var buffer = buffers.shift();
            sourceBuffer.appendBuffer(buffer);
        }

        mediaSource.addEventListener(
            'sourceopen',
            function () {
                sourceBuffer = mediaSource.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
                sourceBuffer.addEventListener(
                    "updateend",
                    function () {
                        updating = false;
                        if (!started_play) {
                            buffer_count += 1;
                            if (buffer_count > 2) {
                                started_play = true;
                                videoElem.currentTime = 4;
                                videoElem.play();             
                            }
                        }
                        notify();
                    }
                );
            }
        );

        var reader = new FileReader();
        reader.addEventListener(
            "load",
            function() {
                buffers.push(this.result);
                if (skipped) {
                    notify();
                    return;
                }

                if (buffers.length < 3) {
                    return;
                }

                var buffer = buffers.shift();
                var header = buffer.slice(0, parse_header_size(buffer));
                buffers.shift();
                buffers.unshift(header);
                skipped = true;
                notify();
            }
        );

        navigator.mediaDevices.getUserMedia(
            { video: true }
        ).then(
            function(stream) {
                var mediaRecorder = new MediaRecorder(stream);
                mediaRecorder.addEventListener(
                    "dataavailable",
                    function(event) {
                        reader.readAsArrayBuffer(event.data);
                    }
                );
                mediaRecorder.start(2000);
            }
        );
    }
);
