function onload() {
    var resultsElem = document.getElementById("results");
    var xhr = new XMLHttpRequest()
    xhr.open("GET", "http://127.0.0.1:8001/");
    xhr.responseType = "json";
    xhr.addEventListener(
        "load",
        function () {
            for (channel of xhr.response) {
                var aElem = document.createElement("a");
                aElem.href = "/watch.html?" + channel.id;
                aElem.appendChild(document.createTextNode(decodeURIComponent(channel.name)));
                resultsElem.appendChild(aElem);
            }
        }
    );
    xhr.send();
}

window.addEventListener("load", onload);
