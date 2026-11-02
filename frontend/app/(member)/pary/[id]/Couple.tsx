'use client';

import React from 'react';


import { CoupleDocument, UpdateCoupleDocument } from '@/graphql/Memberships';
import { PageHeader } from '@/ui/PageHeader';
import { ImageEditorDialog } from '@/ui/ImageEditorDialog';
import { formatOpenDateRange } from '@/ui/format';
import Link from 'next/link';
import { useMutation, useQuery } from 'urql';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';
import { ActivityList } from '@/ui/ActivityList';
import { CompetitionHistory, CompetitionSuggestions } from '@/ui/Competitions';
import { FederationLink } from '@/ui/federation-links';
import { FederationRankBadge, FederationRankings } from '@/ui/FederationRankings';
import { formatAgeGroup } from '@/ui/csts';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { DefinitionList } from '@/ui/DefinitionList';

export function Couple({ id }: { id: string }) {
  const [editingImage, setEditingImage] = React.useState(false);
  const [, updateImage] = useMutation(UpdateCoupleDocument);
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id }, pause: !id });
  const item = data?.couple;
  const actions = useActions(coupleActions, item);
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

  if (!item) return null;
  const personIds = [item.manId, item.womanId];

  return (
    <>
      {editingImage && (
        <ImageEditorDialog
          file={item.image}
          onClose={() => setEditingImage(false)}
          onSave={async (imageId) => {
            const result = await updateImage({ input: { id, patch: { imageId } } });
            if (result.error) throw result.error;
          }}
        />
      )}
      <PageHeader
        imageUrl={item.image?.url}
        onEditImage={actions.some((action) => action.id === 'couple.edit') ? () => setEditingImage(true) : undefined}
        title={item.name}
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


      <DefinitionList
        className="mb-2 text-sm tabular"
        items={{
          Partner: !item.man ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.man.id}`}>
              {item.man?.name}
            </Link>
          ),
          Partnerka: !item.woman ? (
            '?'
          ) : (
            <Link className="underline font-medium" href={`/clenove/${item.woman.id}`}>
              {item.woman?.name}
            </Link>
          ),
          Aktivní: formatOpenDateRange(item)
        }}
      />

      <TabMenu selected={tab} onSelect={setTab}>
        <Tab id="activity" title="Tréninky">
          <ActivityList personIds={personIds} />
        </Tab>

        <Tab id="fed" title="Soutěže">
          <div className="flex flex-col gap-4">
            <FederationCategoryProgress competitors={item.federationCompetitors.nodes} />
            <FederationRankings entries={item.federationRanklistEntries.nodes} />
            <CompetitionHistory competitor={{ coupleId: id }} />
          </div>
        </Tab>
        <Tab id="eligible" title="Možné soutěže">
          <CompetitionSuggestions competitor={{ coupleId: id }} />
        </Tab>
      </TabMenu>
    </>
  );
}
