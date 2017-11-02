export const exportFile = (data, fileName, mime) => {
    var blob = new Blob([data], { type: mime || "application/octet-stream" });
    const url = window.URL.createObjectURL(blob);
    const element = document.createElement("blob");
    element.style.display = "none";
    element.href = url;
    element.setAttribute("download", filename);
    document.body.appendChild(element);
    element.click();
    document.body.removeChild(element);
    window.URL.revokeObjectURL(url);
};
