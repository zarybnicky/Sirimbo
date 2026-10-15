'use client';

import { CoupleDocument } from '@/graphql/Memberships';
import { PageHeader } from '@/ui/PageHeader';
import { formatCoupleName, formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import { useQuery } from 'urql';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { CompetitionHistory } from '@/ui/Competitions';
import { FederationLink } from '@/ui/federation-links';
import { FederationRankBadge, FederationRankings } from '@/ui/FederationRankings';
import { formatAgeGroup } from '@/ui/csts';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';

export function Couple({ id }: { id: string }) {
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  const actions = useActions(coupleActions, item);
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  if (!item) return null;
  const personIds = [item.man, item.woman].flatMap((person) =>
    person ? [person.id] : [],
  );
  const title = formatCoupleName(item);

  return (
    <>
      <PageHeader
        title={title}
        breadcrumbLabel="Pár"
        breadcrumbs={[{ label: 'Páry', href: '/pary' }]}
        actions={actions}
        subtitle={
          <span className="inline-flex flex-wrap items-center gap-2">
            {formatAgeGroup(item.ageGroup)}
            <FederationRankBadge entries={item.federationRanklistEntries.nodes} />
            {item.federationCompetitors.nodes.map((competitor) => (
              <FederationLink
                entity="competitors"
                key={competitor.id}
                federation={competitor.federation}
                externalId={competitor.externalId}
                className="text-accent-12 hover:text-accent-11"
              >
                {competitor.federation === 'csts'
                  ? 'ČSTS'
                  : competitor.federation.toUpperCase()}
              </FederationLink>
            ))}
          </span>
        }
      />

      <div className="mb-2">
        <div>
          Partner:{' '}
          {!item.man ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.man.id}`}>
              {item.man?.name}
            </Link>
          )}
        </div>
        <div>
          Partnerka:{' '}
          {!item.woman ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.woman.id}`}>
              {item.woman?.name}
            </Link>
          )}
        </div>
        <div>{formatOpenDateRange(item)}</div>
        <div>{item.status === 'ACTIVE' ? 'Aktivní pár' : 'Ukončené partnerství'}</div>
      </div>

      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="activity" title="Aktivita">
          <ActivityTimeline personIds={personIds} includeJudging />
        </Tab>

        <Tab id="fed" title="Soutěžní">
          {item.federationCompetitors.nodes.some(
            (competitor) => competitor.categoryProgressesList.length > 0,
          ) && (
            <div className="my-4">
              <FederationCategoryProgress
                competitors={item.federationCompetitors.nodes}
              />
            </div>
          )}
          <FederationRankings entries={item.federationRanklistEntries.nodes} />
          <CompetitionHistory coupleId={id} />
        </Tab>
      </TabMenu>
    </>
  );
}
