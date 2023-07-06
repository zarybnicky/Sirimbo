import React from 'react';
import type { MapProps } from './Map-client';

const Map = React.lazy<React.ComponentType<MapProps>>(() => import('./Map-client'));
export default Map;
