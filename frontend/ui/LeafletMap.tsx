'use client';

import dynamic from 'next/dynamic';

export type MapProps = {
  name: string;
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
};

export const LeafletMap = dynamic<MapProps>(
  async () => {
    const { MapContainer, Marker, Popup, TileLayer } = await import('react-leaflet');
    function LeafletMapClient({ map, name }: MapProps) {
      return (
        <MapContainer
          className="size-[200px]"
          center={map}
          zoom={map.zoom}
          scrollWheelZoom={false}
        >
          <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
          <Marker position={map}>
            <Popup>{name}</Popup>
          </Marker>
        </MapContainer>
      );
    }
    return LeafletMapClient;
  },
  { ssr: false, loading: () => <div className="size-[200px]" /> },
);
