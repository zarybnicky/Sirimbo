'use client';

import { Table } from '@/ui/Table';
import Link from 'next/link';
import { usePaginatedQuery } from '@/lib/usePaginatedQuery';
import { Pagination } from '@/ui/Pagination';
import { CrawlerResponsesDocument } from '@/graphql/Crawler';
import { numericFullFormatter } from '@/ui/format';

export function Responses({ id }: { id: string }) {
  const { data, fetching, error, pagination } = usePaginatedQuery({
    query: CrawlerResponsesDocument,
    variables: (offset) => ({ id, offset }),
    limit: 50,
    getTotal: (data) => data?.crawlerTarget?.responses.totalCount,
  });
  const frontier = data?.crawlerTarget;
  const responses = frontier?.responses.nodes ?? [];

  return (
    <>
      <h2 className="mt-4 mb-2 text-lg font-bold">Responses</h2>
      <Table
        busy={fetching}
        error={error}
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
          <tr key={response.id} className="border-t border-neutral-4 align-top">
            <td className="whitespace-nowrap">
              {response.contentHash ? (
                <Link
                  className="text-accent-11 underline"
                  href={`/admin/crawler/${id}/responses/${response.id}`}
                  prefetch={false}
                >
                  {response.id}
                </Link>
              ) : (
                response.id
              )}
              {response.id === frontier?.lastResponseId && <div>latest</div>}
              {response.id === frontier?.lastSuccessfulResponseId && (
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
      <Pagination {...pagination} />
    </>
  );
}
