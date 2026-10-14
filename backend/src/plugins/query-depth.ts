import { depthLimit } from '@graphile/depth-limit';

export const queryDepthRule = depthLimit({
  maxDepth: 12,
  maxListDepth: 4,
  maxSelfReferentialDepth: 2,
  revealDetails: true,
});
