"use strict";

var mediaRecorder;
var videoElem;
var mediaSource;
var sourceBuffer;
var buffers = [];
var started = false;
var buffer_count = 0;
var started_play = false;

function on_load1() {
    videoElem = document.getElementById("video");
    navigator.mediaDevices.getUserMedia(
        { video: true }
    ).then(
        function(stream) {
            videoElem.src = URL.createObjectURL(stream);
            videoElem.play();
        }
    );
}


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

    var length = get_vint_length(array[segment_offset+4]);
    var offset = segment_offset + 4 + length;

    while (view.getUint32(offset, false) != 0x1F43B675) {
        offset += get_vint_length(array[offset]);
        var length = get_vint_length(array[offset]);
        var size = get_vint_value(array, offset, length);
        offset += length + size;
    }

    return offset;
}


function on_load2() {
    videoElem = document.getElementById("video");
    mediaSource = new MediaSource();
    videoElem.src = URL.createObjectURL(mediaSource);

    mediaSource.addEventListener(
        'sourceopen',
        function () {
            sourceBuffer = mediaSource.addSourceBuffer('video/webm; codecs="vp8,vorbis"');
            sourceBuffer.addEventListener(
                'updateend',
                function() {
                    if (buffers.length > 0) {
                        var buffer = buffers.shift();
                        sourceBuffer.appendBuffer(buffer);
                    }

                    if (started_play) {
                        return;
                    }

                    buffer_count += 1;
                    if (buffer_count > 3) {
                        videoElem.currentTime = 8.0;
                        videoElem.play();
                        started_play = true;
                    }
                }
            );
        }
    );

    navigator.mediaDevices.getUserMedia(
        { video: true }
    ).then(
        function(stream) {
            var mediaRecorder = new MediaRecorder(stream);
            mediaRecorder.ondataavailable = function(event) {
                console.log(event);

                var reader = new FileReader();
                reader.onload = function() {
                    buffers.push(this.result);
                    if (started) {
                        if (buffers.length > 1) {
                            var buffer = buffers.shift();
                            sourceBuffer.appendBuffer(buffer);
                        }
                        return;
                    }

                    if (buffers.length > 6) {
                        var buffer = buffers.shift();
                        var header_size = parse_header_size(buffer);
                        sourceBuffer.appendBuffer(buffer.slice(0, header_size));

                        buffers.shift();
                        buffers.shift();
                        buffers.shift();
                        started = true;
                    }

                };
                reader.readAsArrayBuffer(event.data);
            }
            mediaRecorder.start(2000);
        }
    );
}

window.addEventListener("load", on_load2);
