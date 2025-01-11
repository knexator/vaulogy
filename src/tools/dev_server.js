const fs = require("fs");
const http = require("http");
const WebSocket = require("ws");
const path = require("path");

const static_dir = process.argv[2];

// Create an HTTP server
const server = http.createServer((req, res) => {
  // Build the file path
  let filePath = path.join(
    static_dir,
    req.url === "/" ? "index.html" : req.url,
  );

  // Get the file's extension
  const ext = path.extname(filePath);

  // Default Content-Type
  let contentType = "text/html";

  // Map file extensions to content types
  const mimeTypes = {
    ".html": "text/html",
    ".css": "text/css",
    ".js": "application/javascript",
    ".json": "application/json",
    ".png": "image/png",
    ".jpg": "image/jpeg",
    ".gif": "image/gif",
    ".svg": "image/svg+xml",
    ".wav": "audio/wav",
    ".mp4": "video/mp4",
    ".woff": "application/font-woff",
    ".ttf": "application/font-ttf",
    ".eot": "application/vnd.ms-fontobject",
    ".otf": "application/font-otf",
    ".wasm": "application/wasm",
  };

  // Set Content-Type based on file extension
  contentType = mimeTypes[ext] || contentType;

  // Check if the file exists
  fs.readFile(filePath, (err, content) => {
    if (err) {
      if (err.code === "ENOENT") {
        // File not found
        res.writeHead(404, { "Content-Type": "text/html" });
        res.end("<h1>404 - File Not Found</h1>", "utf-8");
      } else {
        // Other server error
        res.writeHead(500);
        res.end(`Server Error: ${err.code}`);
      }
    } else {
      // Serve the file
      res.writeHead(200, { "Content-Type": contentType });
      res.end(content, "utf-8");
    }
  });
});
// const ws = new WebSocket('ws://' + location.host);
// ws.onmessage = (event) => {
//   if (event.data === 'reload') {
//     console.log('File changed, reloading...');
//     location.reload();
//   }
// };

// Attach WebSocket server
const wss = new WebSocket.Server({ server });

// Watch the file for changes
fs.watch(static_dir, (eventType) => {
  if (eventType === "change") {
    // console.log(`${static_dir} changed, notifying clients...`);
    // Notify all connected clients
    wss.clients.forEach((client) => {
      if (client.readyState === WebSocket.OPEN) {
        client.send("reload");
      }
    });
  }
});

// Start the server
const PORT = 3000;
server.listen(PORT, () => {
  console.log(`Node server is running on http://localhost:${PORT}`);
});
