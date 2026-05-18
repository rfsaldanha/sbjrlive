# sbjrlive

Live Shiny dashboard for ADS-B traffic around SBJR.

## ADS-B sources

The app pools ADSB.lol, Airplanes.live, and adsb.fi by default, then deduplicates aircraft across sources.

Extra compatible sources can be added without editing `app.R`:

```sh
ADSB_EXTRA_PROVIDERS="My feed=https://example.com/api/v2|latlon;Other feed=https://example.com/v2|point"
```

Supported path styles:

- `latlon`: `/lat/{lat}/lon/{lon}/dist/{radius}`
- `point`: `/point/{lat}/{lon}/{radius}`
