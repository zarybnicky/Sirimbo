import Link from 'next/link';
import { notFound } from 'next/navigation';
import { Layout } from '@/ui/Layout';
import { PageHeader } from '@/ui/TitleBar';
import { numericDateWithYearFormatter, numericFullFormatter } from '@/ui/format';
import { getRequestContext } from '@/lib/server/tenant';
import { runQuery } from '@/lib/server/postgresql';
import {
  browseEvent,
  browseCompetition,
  browseParticipants,
  browseParticipantPeople,
  browseRounds,
  browseSourceFrontiers,
} from '../../../federated.queries';

export const metadata = {
  title: 'Federated competition',
  robots: { index: false, follow: false },
};

export default async function CompetitionPage({
  params,
}: {
  params: Promise<{ federation: string; eventId: string; competitionId: string }>;
}) {
  const { auth } = await getRequestContext();
  if (!auth.isSystemAdmin) return <Layout requireSystemAdmin />;
  const { federation, eventId, competitionId } = await params;
  if (!/^[0-9]{1,18}$/.test(eventId) || !/^[0-9]{1,18}$/.test(competitionId)) notFound();
  const [[event], [competition]] = await Promise.all([
    runQuery(browseEvent, { federation, eventId }),
    runQuery(browseCompetition, { eventId, competitionId }),
  ]);
  if (!event || !competition) notFound();
  const [participants, people, rounds, sources] = await Promise.all([
    runQuery(browseParticipants, { competitionId }),
    runQuery(browseParticipantPeople, { competitionId }),
    runQuery(browseRounds, { competitionId }),
    runQuery(browseSourceFrontiers, {
      federation,
      eventKey: event.external_id,
      competitionKey: competition.external_id,
    }),
  ]);
  const peopleByCompetitor = Object.groupBy(people, (person) => person.competitor_id);
  return (
    <Layout requireSystemAdmin>
      <PageHeader
        title={competition.category}
        breadcrumbs={[
          { label: 'Federations', href: '/admin/federated' },
          { label: event.federation_name, href: `/admin/federated/${federation}` },
          {
            label: event.name || event.external_id,
            href: `/admin/federated/${federation}/${eventId}`,
          },
          { label: competition.category },
        ]}
      />
      <dl className="text-sm tabular">
        <dt>competition ID</dt>
        <dd>{competition.external_id}</dd>
        <dt>type</dt>
        <dd>{competition.competition_type || '-'}</dd>
        <dt>date</dt>
        <dd>{numericDateWithYearFormatter.format(competition.start_date)}</dd>
        <dt>check-in end</dt>
        <dd>{competition.check_in_end || '-'}</dd>
        <dt>participants reported</dt>
        <dd>{competition.participants_total ?? '-'}</dd>
        <dt>completed</dt>
        <dd>
          {competition.completed_at
            ? numericFullFormatter.format(competition.completed_at)
            : '-'}
        </dd>
      </dl>
      <h2 className="mt-4 mb-2 text-lg font-bold">Registrations and results</h2>
      <div className="col-full-width overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {[
                'competitor',
                'registration',
                'rank',
                'start number',
                'points',
                'final gain',
                'status',
                'local matches',
              ].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {participants.map((participant) => {
              const matches = peopleByCompetitor[participant.competitor_id] ?? [];
              const couples = [
                ...new Set(matches.flatMap((person) => person.couple_ids ?? [])),
              ];
              return (
                <tr
                  key={participant.competitor_id}
                  className="border-t border-neutral-4 align-top"
                >
                  <th scope="row" className="px-2 py-1 font-normal">
                    <Link
                      className="text-accent-11 underline"
                      href={`/admin/federated/${federation}/competitors/${participant.competitor_external_id}`}
                      prefetch={false}
                    >
                      {participant.name || participant.competitor_id}
                    </Link>
                    <div className="text-xs">{participant.competitor_id}</div>
                  </th>
                  <td className="px-2 py-1">
                    {participant.cancelled === null
                      ? '-'
                      : participant.cancelled
                        ? 'cancelled'
                        : 'registered'}
                  </td>
                  <td className="px-2 py-1 whitespace-nowrap">
                    {participant.ranking}
                    {participant.ranking_to &&
                    participant.ranking_to !== participant.ranking
                      ? `–${participant.ranking_to}`
                      : ''}
                  </td>
                  <td className="px-2 py-1">{participant.start_number}</td>
                  <td className="px-2 py-1">{participant.point_gain}</td>
                  <td className="px-2 py-1">{participant.final_gain}</td>
                  <td className="px-2 py-1">{participant.completion_status}</td>
                  <td className="px-2 py-1">
                    {matches.map((person) => (
                      <div key={`${person.federated_person_id}:${person.person_id}`}>
                        <Link
                          className="font-mono text-xs text-accent-11 underline"
                          href={`/admin/federated/${federation}/people/${person.person_external_id}`}
                          prefetch={false}
                        >
                          {person.federated_person_id}
                        </Link>
                        {' → '}
                        {person.person_id ? (
                          <>
                            <Link
                              className="text-accent-11 underline"
                              href={`/clenove/${person.person_id}`}
                            >
                              {person.person_name}
                            </Link>{' '}
                            ({person.tenants?.join(', ') || 'No active membership'})
                          </>
                        ) : (
                          'No local match'
                        )}
                      </div>
                    ))}
                    {matches.length === 0 && 'No competitor members'}
                    {couples.map((id) => (
                      <div key={id}>
                        <Link className="text-accent-11 underline" href={`/pary/${id}`}>
                          Couple {id}
                        </Link>
                      </div>
                    ))}
                  </td>
                </tr>
              );
            })}
            {participants.length === 0 && (
              <tr>
                <td colSpan={8} className="px-2 py-1">
                  No registrations or results
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <h2 className="mt-4 mb-2 text-lg font-bold">Rounds</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-left text-sm tabular-nums">
          <thead>
            <tr>
              {['round', 'key', 'scoring method', 'results'].map((label) => (
                <th key={label} scope="col" className="px-2 py-1">
                  {label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rounds.map((round) => (
              <tr key={round.id} className="border-t border-neutral-4">
                <th scope="row" className="px-2 py-1 font-normal">
                  {round.round_label || round.round_key}
                </th>
                <td className="px-2 py-1">{round.round_key}</td>
                <td className="px-2 py-1">{round.scoring_method}</td>
                <td className="px-2 py-1">{round.results}</td>
              </tr>
            ))}
            {rounds.length === 0 && (
              <tr>
                <td colSpan={4} className="px-2 py-1">
                  No rounds
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      <h2 className="mt-4 mb-2 text-lg font-bold">Crawler</h2>
      {sources.map((source) => (
        <p key={source.id}>
          <Link
            className="text-accent-11 underline"
            href={`/admin/crawler/${source.id}`}
            prefetch={false}
          >
            {federation}:{source.kind}:{source.key}
          </Link>{' '}
          — fetch: {source.fetch_status}, process: {source.process_status}
        </p>
      ))}
      {sources.length === 0 && <p>No matching frontiers</p>}
    </Layout>
  );
}
