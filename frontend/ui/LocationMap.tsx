'use client';

import dynamic from 'next/dynamic';

export type LocationMapProps = {
  map: {
    lat: number;
    lng: number;
    zoom: number;
  };
};

export const LocationMap = dynamic<LocationMapProps>(
  async () => {
    const { Map, Marker } = await import('pigeon-maps');

    function LocationMapClient({ map }: LocationMapProps) {
      const center: [number, number] = [map.lat, map.lng];

      return (
        <Map
          width={200}
          height={200}
          defaultCenter={center}
          defaultZoom={map.zoom}
          metaWheelZoom
          attributionPrefix={false}
        >
          <Marker anchor={center} />
        </Map>
      );
    }

    return LocationMapClient;
  },
  { ssr: false, loading: () => <div className="size-[200px]" /> },
);
