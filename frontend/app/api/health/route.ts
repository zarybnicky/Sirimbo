import { TenantSettingsDocument } from '@/graphql/Tenant';
import { executeGraphql } from '@/lib/server/graphql';
import { resolveClaims } from '@/lib/server/tenant';
import { defaultTenant } from '@/tenant/catalog';

type DeploymentResponse = {
  ok: boolean;
  deploymentId: string | null;
};

let pending: Promise<unknown> | undefined;

export async function GET() {
  let ok = false;
  let timeout: ReturnType<typeof setTimeout> | undefined;
  try {
    pending ??= executeGraphql(
      TenantSettingsDocument,
      { tenantId: String(defaultTenant.id) },
      resolveClaims(null, String(defaultTenant.id)),
    ).finally(() => { pending = undefined; });
    await Promise.race([
      pending,
      new Promise((_, reject) => {
        timeout = setTimeout(() => reject(new Error('GraphQL readiness timed out')), 2000);
      }),
    ]);
    ok = true;
  } catch {
    // Keep the probe response independent of database or schema error details.
  } finally {
    clearTimeout(timeout);
  }
  return Response.json(
    {
      ok,
      deploymentId: process.env.NEXT_DEPLOYMENT_ID || null,
    } satisfies DeploymentResponse,
    {
      status: ok ? 200 : 503,
      headers: {
        'Cache-Control': 'private, no-store, max-age=0, must-revalidate',
      },
    },
  );
}
