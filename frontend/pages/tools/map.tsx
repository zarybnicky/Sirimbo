import React from "react";
import * as Plot from '@observablehq/plot';

export default function EventMap() {
  const [cz, setCz] = React.useState<object | null>(null);
  const [events, setEvents] = React.useState<Array<{ start_date: string; venue_lat: number; venue_lng: number; }> | null>(null);
  const containerRef = React.useRef<HTMLDivElement | null>(null);

  React.useEffect(() => {
    Promise.all([
      fetch('/cz-admin1.json').then(x => x.json()).then(setCz),
      fetch('https://api.rozpisovnik.cz/federated/event?federation=eq.csts&start_date=lte.' + new Date().toISOString().slice(0, 10))
        .then(x => x.json()).then(setEvents),
    ]);
  }, []);

  React.useEffect(() => {
    console.log(cz, events, containerRef);
    if (!cz || !events || !containerRef.current) return;

    const plot = Plot.plot({
      width: 1024,
      projection: {
        type: "azimuthal-equidistant",
        domain: cz,
        inset: 10,
        rotate: [-20],
      },
      axis: null,
      color: {
        type: 'categorical',
        scheme: 'sinebow',
        domain: [1,2,3,4,5,6,7,8,9,10,11,12],
        legend: true,
      },
      marks: [
        Plot.geo(cz, { stroke: '#aaa' }),
        Plot.dot(events, {
          y: "venue_lat",
          x: "venue_lng",
          fill: x => Number.parseInt(x.start_date.slice(5, 7)),
          r: 2,
          fx: x => (1 + Number.parseInt(x.start_date.slice(0, 4))) % 2,
          fy: x => Math.floor((1 + Number.parseInt(x.start_date.slice(0, 4))) / 2),
          channels: {
            title: x => [new Date(x.start_date).toLocaleDateString(), x.name, x.location].join('\n'),
          },
          tip: true,
        }),
        Plot.text(events, {
          dx: -20,
          dy: 20,
          frameAnchor: "top-right",
          text: x => x.start_date.slice(0, 4),
          fx: x => (1 + Number.parseInt(x.start_date.slice(0, 4))) % 2,
          fy: x => Math.floor((1 + Number.parseInt(x.start_date.slice(0, 4))) / 2),
        }),
      ]
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [cz, events]);

  return <div ref={containerRef} />
}
