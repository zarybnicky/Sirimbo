import React from 'react';
import L from 'leaflet';
import * as ReactLeaflet from 'react-leaflet';
import iconRetinaUrl from 'leaflet/dist/images/marker-icon-2x.png';
import iconUrl from 'leaflet/dist/images/marker-icon.png';
import shadowUrl from 'leaflet/dist/images/marker-shadow.png';

export type MapProps = {
  children: (x: typeof ReactLeaflet) => React.ReactElement;
} & Omit<ReactLeaflet.MapContainerProps, 'children'>;

function Map({ children, ...rest }: MapProps) {
  React.useEffect(() => {
    (function init() {
      // eslint-disable-next-line
      delete (L.Icon.Default.prototype as any)._getIconUrl;

      L.Icon.Default.mergeOptions({
        iconRetinaUrl: typeof iconRetinaUrl === 'object' ? iconRetinaUrl['src'] : iconRetinaUrl,
        iconUrl: typeof iconUrl === 'object' ? iconUrl['src'] : iconUrl,
        shadowUrl: typeof shadowUrl === 'object' ? shadowUrl['src'] : shadowUrl,
      });
    })();
  }, []);

  return (
    <ReactLeaflet.MapContainer {...rest}>
      {children(ReactLeaflet)}
    </ReactLeaflet.MapContainer>
  );
};

export default Map;
