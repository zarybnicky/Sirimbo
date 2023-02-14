import { useEffect } from 'react';
import L from 'leaflet';
import * as ReactLeaflet from 'react-leaflet';
import 'leaflet/dist/leaflet.css';

import iconRetinaUrl from 'leaflet/dist/images/marker-icon-2x.png';
import iconUrl from 'leaflet/dist/images/marker-icon.png';
import shadowUrl from 'leaflet/dist/images/marker-shadow.png';

const Map: React.FC<{
  children: (x: typeof ReactLeaflet) => React.ReactElement;
} & ReactLeaflet.MapContainerProps> = ({ children, ...rest }) => {
  useEffect(() => {
    (async function init() {
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
  )
}

export default Map;
