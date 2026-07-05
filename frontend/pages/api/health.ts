import type { NextApiRequest, NextApiResponse } from 'next';

type DeploymentResponse = {
  ok: boolean;
  deploymentId: string | null;
};

const deploymentId = process.env.NEXT_DEPLOYMENT_ID || null;

export default function health(
  _req: NextApiRequest,
  res: NextApiResponse<DeploymentResponse>,
) {
  res.setHeader('Cache-Control', 'private, no-store, max-age=0, must-revalidate');
  res.status(200).json({ ok: true, deploymentId });
}
