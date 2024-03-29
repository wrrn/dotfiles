module.exports = {
    defaultBrowser: "Firefox Developer Edition",
    options: {
        // Hide the icon in the top bar.
        hideIcon: true
    },
    handlers: [
        {
            match: finicky.matchHostnames(["meet.google.com"]),
            browser: "Google Chrome"
        },
        {
            match: "open.spotify.com/*",
            browser: "Spotify"
        } 
    ]
}
