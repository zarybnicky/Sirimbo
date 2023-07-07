import dynamic from 'next/dynamic';
import type { MapProps } from './Map-client';

const Map = dynamic<MapProps>(() => import('./Map-client'), { ssr: false });
export default Map;
