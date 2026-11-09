import { DefinitionList } from '@/ui/DefinitionList';
import { notFound, redirect } from 'next/navigation';
import Link from 'next/link';
import { executeGraphql } from '@/lib/server/graphql';
import { CrawlerTargetDocument } from '@/graphql/Crawler';
import { getRequestContext } from '@/lib/server/tenant';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/PageHeader';
import { Table } from '@/ui/Table';
import { Pagination } from '@/ui/Pagination';
import { formatDateOnly, numericFullFormatter } from '@/ui/format';
import { Metadata } from 'next';
import { CrawlerTargets } from '../Crawler';

const pageSize = 50;

export const metadata: Metadata = {
  title: 'Crawler',
  robots: { index: false, follow: false },
};

export default async function FrontierPage({
  params,
  searchParams,
}: {
  params: Promise<{ id: string }>;
  searchParams: Promise<{ page?: string | string[] }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;

  const [{ id: identifier }, search] = await Promise.all([params, searchParams]);
  const [federation, kind, ...keyParts] = decodeURIComponent(identifier).split(':');
  if (!federation || !kind) notFound();
  if (keyParts.length === 0) {
    return (
      <Layout requireSystemAdmin>
        <CrawlerTargets federation={federation} kind={kind} />
      </Layout>
    );
  }
  const key = keyParts.join(':');
  const rawPage = Array.isArray(search.page) ? search.page[0] : search.page;
  const page = Math.max(1, Number.parseInt(rawPage ?? '1', 10) || 1);
  const { crawlerTarget: frontier, crawlerFetchJobs } = await executeGraphql(
    CrawlerTargetDocument,
    {
      federation,
      kind,
      key,
      offset: Math.min((page - 1) * pageSize, 2_147_483_647),
    },
  );
  if (!frontier) notFound();

  const job = crawlerFetchJobs?.nodes[0];
  const { nodes: responses, totalCount } = frontier.responses;
  const lastPage = Math.max(1, Math.ceil(totalCount / pageSize));
  const basePath = `/admin/crawler/${federation}:${kind}:${encodeURIComponent(key)}`;
  if (page > lastPage) {
    redirect(lastPage > 1 ? `${basePath}?page=${lastPage}` : basePath);
  }

  const title = `${frontier.federation}:${frontier.kind}${frontier.key ? `:${frontier.key}` : ''}`;

  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={title}
        breadcrumbLabel="Frontier"
        breadcrumbs={[
          { title: 'Crawler', href: '/admin/crawler' },
          {
            title: `${frontier.federation}:${frontier.kind}`,
            href: `/admin/crawler/${frontier.federation}:${frontier.kind}`,
          },
        ]}
      />
      <DefinitionList
        items={{
          id: frontier.id,
          'fetch status': frontier.fetchStatus.toLowerCase(),
          'process status': frontier.processStatus.toLowerCase(),
          'error count': frontier.errorCount,
          discovered: frontier.discoveredAt
            ? numericFullFormatter.format(new Date(frontier.discoveredAt))
            : '-',
          'reference date': frontier.referenceDate
            ? formatDateOnly(frontier.referenceDate)
            : '-',
          'last fetched': frontier.lastFetchedAt
            ? numericFullFormatter.format(new Date(frontier.lastFetchedAt))
            : '-',
          'next fetch': frontier.nextFetchAt
            ? numericFullFormatter.format(new Date(frontier.nextFetchAt))
            : '-',
          'last process error': frontier.lastProcessErrorAt
            ? numericFullFormatter.format(new Date(frontier.lastProcessErrorAt))
            : '-',
        }}
      />
      {frontier.lastProcessError && (
        <pre className="whitespace-pre-wrap break-all text-sm">
          {frontier.lastProcessError}
        </pre>
      )}

      {job && (
        <>
          <h2 className="mt-4 mb-2 text-lg font-bold">Fetch job</h2>
          <DefinitionList
            items={{
              id: job.jobId,
              state: job.state === 'locked' ? 'running' : job.state,
              scheduled: job.runAt
                ? numericFullFormatter.format(new Date(job.runAt))
                : '-',
              'locked since': job.lockedAt
                ? numericFullFormatter.format(new Date(job.lockedAt))
                : '-',
              attempts: `${job.attempts ?? '-'} / ${job.maxAttempts ?? '-'}`,
              'worker error': job.jobError && (
                <pre className="whitespace-pre-wrap break-all text-sm">
                  {job.jobError}
                </pre>
              ),
            }}
          />
        </>
      )}

      <h2 className="mt-4 mb-2 text-lg font-bold">Responses</h2>
      <Table
        columns={[
          'response',
          'http',
          'sys_period',
          'last observed',
          'observations',
          'url / error',
        ]}
        className="col-feature"
        empty="No responses"
      >
        {responses.map((response) => (
          <tr key={response.id} className="align-top">
            <td className="whitespace-nowrap">
              {response.contentHash ? (
                <Link
                  className="text-accent-11 underline"
                  href={`${basePath}/responses/${response.id}`}
                  prefetch={false}
                >
                  {response.id}
                </Link>
              ) : (
                response.id
              )}
              {response.id === frontier.lastResponseId && <div>latest</div>}
              {response.id === frontier.lastSuccessfulResponseId && (
                <div>last successful</div>
              )}
            </td>
            <td>{response.httpStatus ?? '-'}</td>
            <td className="whitespace-nowrap">
              {response.sysPeriod?.start?.value ? (
                <>
                  <div>
                    [
                    {numericFullFormatter.format(
                      new Date(response.sysPeriod?.start?.value),
                    )}
                    ,
                  </div>
                  <div>
                    {response.sysPeriod?.end?.value
                      ? numericFullFormatter.format(
                          new Date(response.sysPeriod?.end?.value),
                        )
                      : '∞'}
                    )
                  </div>
                </>
              ) : (
                <div>
                  {numericFullFormatter.format(new Date(response.fetchedAt))} (fetched)
                </div>
              )}
            </td>
            <td className="whitespace-nowrap">
              {response.lastObservedAt
                ? numericFullFormatter.format(new Date(response.lastObservedAt))
                : '-'}
            </td>
            <td>{response.observationCount}</td>
            <td className="min-w-64 break-all">
              <div>{response.url}</div>
              {response.error && (
                <details>
                  <summary className="cursor-pointer">Error</summary>
                  <pre className="max-h-72 overflow-auto whitespace-pre-wrap">
                    {response.error}
                  </pre>
                </details>
              )}
            </td>
          </tr>
        ))}
      </Table>
      <Pagination
        total={totalCount}
        limit={pageSize}
        page={page}
        href={(page) => (page <= 1 ? basePath : `${basePath}?page=${page}`)}
      />
    </Layout>
  );
}
