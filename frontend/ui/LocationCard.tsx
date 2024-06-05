import React from 'react';
import LeafletMap from '@/map';

type Props = {
  name: string;
  children: React.ReactNode;
  href: string;
  mapHref: string;
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
};

export function LocationCard(x: Props) {
  return (
    <div>
      <h3 className="text-accent-10 text-2xl font-bold mb-4 mt-8">{x.name}</h3>
      <div className="grid md:grid-cols-[1fr_2fr] gap-4 items-center">
        <LeafletMap
          className="h-[200px]"
          center={x.map}
          zoom={x.map.zoom}
          scrollWheelZoom={false}
        >
          {({ TileLayer, Marker, Popup }) => (
            <>
              <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
              <Marker position={x.map}>
                <Popup>{x.name}</Popup>
              </Marker>
            </>
          )}
        </LeafletMap>

        <div className="grow text-neutral-12">
          <div className="py-2">{x.children}</div>
          <a href={x.href} rel="noreferrer" target="_blank" className="block underline">
            {x.href}
          </a>
          <a
            href={x.mapHref}
            rel="noreferrer"
            target="_blank"
            className="block underline"
          >
            Otevřít mapu
          </a>
        </div>
      </div>
    </div>
  );
};
