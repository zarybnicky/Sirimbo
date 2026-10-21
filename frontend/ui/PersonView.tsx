'use client';

import { PersonMembershipsDocument } from '@/graphql/Person';
import { useActions } from '@/lib/actions';
import { personActions } from '@/lib/actions/person';
import { useAuth } from '@/lib/auth';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { CompetitionHistory } from '@/ui/Competitions';
import { formatAgeGroup, formatClass, getBestCstsProgress } from '@/ui/csts';
import { FederationLink } from '@/ui/federation-links';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import { FederationRankings } from '@/ui/FederationRankings';
import { PageHeader } from '@/ui/PageHeader';
import { PersonActivityBadges } from '@/ui/PersonActivityBadges';
import { PersonMembershipView } from '@/ui/PersonMembershipView';
import { PersonPaymentsView } from '@/ui/PersonPaymentsView';
import { PersonWorkReportView } from '@/ui/PersonWorkReportView';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { SiFacebook, SiInstagram } from '@icons-pack/react-simple-icons';
import { Globe, Music2 } from 'lucide-react';
import { parseAsString, useQueryState } from 'nuqs';
import React from 'react';
import { useQuery } from 'urql';

export const PersonView = React.memo(function PersonView({
  id,
  parent,
}: {
  id: string;
  parent?: { label: string; href: string };
}) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id, includeActivity: auth.isTrainer || auth.isMyPerson(id) },
    pause: !id,
  });
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(id);
  const item = data?.person;

  const actions = useActions(personActions, item);
  const sttProgress = getBestCstsProgress(
    item?.federationCategoryProgresses.nodes,
    'Standard',
  );
  const latProgress = getBestCstsProgress(
    item?.federationCategoryProgresses.nodes,
    'Latin',
  );
  const sttClass = formatClass(sttProgress?.category?.class);
  const latClass = formatClass(latProgress?.category?.class);
  const categoryProgress = [
    formatAgeGroup(item?.ageGroup),
    sttClass || latClass ? `${sttClass ?? '-'}/${latClass ?? '-'}` : null,
  ]
    .filter(Boolean)
    .join(' ');

  const personIds = React.useMemo(() => [id], [id]);

  if (!item) return null;

  const competitors = Object.values(
    Object.groupBy(item.federationCategoryProgresses.nodes, (x) => x.competitorId),
  ).flatMap((progress) => {
    const competitor = progress?.[0]?.competitor;
    return competitor && progress
      ? [{ ...competitor, categoryProgressesList: progress }]
      : [];
  });

  const subtitleParts = [
    <span key="category" className="inline-flex flex-wrap items-center gap-2">
      {categoryProgress}
    </span>,
    item.phone && (
      <a className="underline" href={`tel:${item.phone.replaceAll(' ', '')}`}>
        {item.phone}
      </a>
    ),
    item.email && (
      <a className="underline" href={`mailto:${item.email}`}>
        {item.email}
      </a>
    ),
    ...item.federationPeople.nodes.map((person) => (
      <FederationLink
        entity="people"
        key={person.id}
        federation={person.federation}
        externalId={person.externalId}
        className="text-accent-12 hover:text-accent-11"
      >
        {person.federation === 'csts' ? 'ČSTS' : person.federation.toUpperCase()}
      </FederationLink>
    )),
  ].filter(Boolean);

  const socialLinks = [
    {
      id: 'instagram',
      label: 'Instagram',
      text: item.instagramUsername ? `@${item.instagramUsername}` : null,
      href: item.instagramUsername
        ? `https://www.instagram.com/${encodeURIComponent(item.instagramUsername)}`
        : null,
      Icon: SiInstagram,
    },
    {
      id: 'tiktok',
      label: 'TikTok',
      text: item.tiktokUsername ? `@${item.tiktokUsername}` : null,
      href: item.tiktokUsername
        ? `https://www.tiktok.com/@${encodeURIComponent(item.tiktokUsername)}`
        : null,
      Icon: Music2,
    },
    {
      id: 'facebook',
      label: 'Facebook',
      text: 'Facebook',
      href:
        item.facebookUrl?.startsWith('https://') ||
        item.facebookUrl?.startsWith('http://')
          ? item.facebookUrl
          : null,
      Icon: SiFacebook,
    },
    {
      id: 'website',
      label: 'Web',
      text: 'Web',
      href:
        item.websiteUrl?.startsWith('https://') || item.websiteUrl?.startsWith('http://')
          ? item.websiteUrl
          : null,
      Icon: Globe,
    },
  ].filter((link) => link.href);

  return (
    <>
      <PageHeader
        title={item.name}
        breadcrumbLabel="Člen"
        breadcrumbs={parent ? [parent] : undefined}
        subtitle={
          subtitleParts.length > 0 ? (
            <span className="inline-flex flex-wrap items-center gap-y-1 [&>*:not(:last-child)]:after:content-['•'] *:after:mx-1.5 *:after:text-neutral-9">
              {subtitleParts.map((part, index) => (
                <span key={index}>{part}</span>
              ))}
            </span>
          ) : undefined
        }
        actions={actions}
      />

      {item.lastActivity && (
        <div className="-mt-2 mb-4">
          <PersonActivityBadges activity={item.lastActivity} />
        </div>
      )}

      {socialLinks.length > 0 && (
        <div className="-mt-2 mb-4 flex flex-wrap gap-2">
          {socialLinks.map(({ id, label, text, href, Icon }) => (
            <a
              key={id}
              href={href!}
              target="_blank"
              rel="noreferrer"
              aria-label={`Otevřít ${label}`}
              className="inline-flex min-h-8 items-center gap-1.5 rounded-md border border-neutral-5 bg-neutral-2 px-2.5 py-1 text-sm text-neutral-12 hover:border-accent-7 hover:bg-accent-3 focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent-8"
            >
              <Icon className="size-4 shrink-0" aria-hidden="true" />
              <span>{text}</span>
            </a>
          ))}
        </div>
      )}

      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="info" title="Členství">
          <PersonMembershipView item={item} />
        </Tab>
        <Tab id="fed" title="Soutěžní">
          {competitors.length > 0 && (
            <div className="my-4">
              <FederationCategoryProgress competitors={competitors} />
            </div>
          )}
          <FederationRankings entries={item.federationRanklistEntries.nodes} />
          <CompetitionHistory personId={id} />
        </Tab>
        {isAdminOrCurrentPerson && (
          <>
            <Tab id="activity" title="Aktivita">
              <ActivityTimeline personIds={personIds} includeJudging />
            </Tab>
            <Tab id="payment" title="Platby">
              <PersonPaymentsView id={id} />
            </Tab>
            <Tab id="workReport" title="Výkaz práce" requireTrainer>
              <PersonWorkReportView id={id} />
            </Tab>
          </>
        )}
      </TabMenu>
    </>
  );
});
