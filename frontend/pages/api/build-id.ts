import { buildId } from '@/lib/build-id';
import type { NextApiRequest, NextApiResponse } from 'next';

const buildIdApi = (_req: NextApiRequest, res: NextApiResponse): void => {
  res.status(200).json({ buildId });
};

export default buildIdApi;
