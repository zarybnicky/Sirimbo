import dynamic from 'next/dynamic';

const Map = dynamic(() => import('./Map-client'), {
  ssr: false,
});
export default Map;
