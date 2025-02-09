import dynamic from 'next/dynamic';
import type { MapProps } from './map.client';

const Map = dynamic<MapProps>(() => import('./map.client'), { ssr: false });
export default Map;
