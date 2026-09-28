'use client';

import React from 'react';
import { PersonMembershipsDocument } from '@/graphql/Person';
import { PageHeader } from '@/ui/TitleBar';
import { formatCstsClass, getBestCstsProgress } from '@/ui/csts';
import { useQuery } from 'urql';
import { useAuth } from '@/lib/auth';
import { formatAgeGroup } from '@/ui/format';
import { parseAsString, useQueryState } from 'nuqs';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { PersonMembershipView } from '@/ui/PersonMembershipView';
import { PersonPaymentsView } from '@/ui/PersonPaymentsView';
import { PersonWorkReportView } from '@/ui/PersonWorkReportView';
import { personActions } from '@/lib/actions/person';
import { useActions } from '@/lib/actions';
import { ActivityTimeline } from '@/ui/ActivityTimeline';
import { CstsPersonLink } from '@/ui/csts-links';
import { Globe, Music2 } from 'lucide-react';
import { SiFacebook, SiInstagram } from '@icons-pack/react-simple-icons';

export const PersonView = React.memo(function PersonView({ id }: { id: string }) {
  const auth = useAuth();
  const [{ data }] = useQuery({
    query: PersonMembershipsDocument,
    variables: { id },
    pause: !id,
  });
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  const isAdminOrCurrentPerson = auth.isAdmin || auth.isMyPerson(id);
  const item = data?.person;

  const actions = useActions(personActions, item);
  const sttProgress = getBestCstsProgress(item?.cstsProgressList, 'Standard');
  const latProgress = getBestCstsProgress(item?.cstsProgressList, 'Latin');
  const sttClass = formatCstsClass(sttProgress?.category?.class);
  const latClass = formatCstsClass(latProgress?.category?.class);
  const categoryProgress = [
    formatAgeGroup(item?.birthDate),
    sttClass || latClass ? `${sttClass ?? '-'}/${latClass ?? '-'}` : null,
  ]
    .filter(Boolean)
    .join(' ');

  const personIds = React.useMemo(() => [id], [id]);

  if (!item) return null;

  const subtitleParts = [
    categoryProgress,
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
    item.cstsId && (
      <CstsPersonLink idt={item.cstsId} className="text-accent-12 hover:text-accent-11">
        ČSTS
      </CstsPersonLink>
    ),
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
