import { useEffect } from 'react';
import L from 'leaflet';
import * as ReactLeaflet from 'react-leaflet';
import 'leaflet/dist/leaflet.css';
import iconRetinaUrl from 'leaflet/dist/images/marker-icon-2x.png';
import iconUrl from 'leaflet/dist/images/marker-icon.png';
import shadowUrl from 'leaflet/dist/images/marker-shadow.png';

export type MapProps =   {
    children: (x: typeof ReactLeaflet) => React.ReactElement;
} & Omit<ReactLeaflet.MapContainerProps, 'children'>

const Map: React.FC<MapProps> = ({ children, ...rest }) => {
  useEffect(() => {
    (function init() {
      // eslint-disable-next-line
      delete (L.Icon.Default.prototype as any)._getIconUrl;

      L.Icon.Default.mergeOptions({
        iconRetinaUrl: iconRetinaUrl.src,
        iconUrl: iconUrl.src,
        shadowUrl: shadowUrl.src,
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
