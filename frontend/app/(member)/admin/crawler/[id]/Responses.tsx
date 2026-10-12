'use client';

import Link from 'next/link';
import { useEffect } from 'react';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import { FormError } from '@/ui/form';
import { Spinner } from '@/ui/Spinner';
import { Pagination } from '@/ui/Pagination';
import { CrawlerResponsesDocument } from '@/graphql/Crawler';
import { numericFullFormatter } from '@/ui/format';

export function Responses({ id }: { id: string }) {
  const [currentPage, setPage] = useQueryState(
    'page',
    parseAsInteger
      .withDefault(1)
      .withOptions({ shallow: true, scroll: false, history: 'push' }),
  );
  const [{ data, fetching, error }] = useQuery({
    query: CrawlerResponsesDocument,
    variables: { id, offset: Math.min(Math.max(0, currentPage - 1) * 50, 2_147_483_647) },
  });
  const frontier = data?.crawlerTarget;
  const total = frontier?.responses.totalCount;
  useEffect(() => {
    if (fetching || error || total == null) return;
    const page = Math.max(1, Math.min(currentPage, Math.ceil(total / 50)));
    if (page !== currentPage) void setPage(page, { history: 'replace' });
  }, [currentPage, total, fetching, error, setPage]);
  if (error) return <FormError error={error} />;
  if (!frontier) return <Spinner />;
  const responses = frontier.responses.nodes;

  return (
    <>
      {fetching && (
        <p role="status" className="my-2 text-sm text-neutral-11">
          Loading…
        </p>
      )}
      <h2 className="mt-4 mb-2 text-lg font-bold">Responses</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'response',
                'http',
                'sys_period',
                'last observed',
                'observations',
                'url / error',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1 whitespace-nowrap">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {responses.map((response) => (
              <tr key={response.id} className="border-t border-neutral-4 align-top">
                <td className="px-2 py-1 whitespace-nowrap">
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
                  {response.id === frontier.lastResponseId && <div>latest</div>}
                  {response.id === frontier.lastSuccessfulResponseId && (
                    <div>last successful</div>
                  )}
                </td>
                <td className="px-2 py-1">{response.httpStatus ?? '-'}</td>
                <td className="px-2 py-1 whitespace-nowrap">
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
                      {numericFullFormatter.format(new Date(response.fetchedAt))}{' '}
                      (fetched)
                    </div>
                  )}
                </td>
                <td className="px-2 py-1 whitespace-nowrap">
                  {response.lastObservedAt
                    ? numericFullFormatter.format(new Date(response.lastObservedAt))
                    : '-'}
                </td>
                <td className="px-2 py-1">{response.observationCount}</td>
                <td className="min-w-64 px-2 py-1 break-all">
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
            {responses.length === 0 && (
              <tr>
                <td colSpan={6} className="px-2 py-1">
                  No responses
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <Pagination
        total={frontier.responses.totalCount}
        limit={50}
        page={currentPage}
        onPageChange={setPage}
      />
    </>
  );
}
