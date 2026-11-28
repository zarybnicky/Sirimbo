'use client';

import React from 'react';

import {
  CoupleDocument,
  type CoupleQuery,
  UpdateCoupleDocument,
} from '@/graphql/Memberships';
import { PageHeader } from '@/ui/PageHeader';
import { ImageEditorDialog } from '@/ui/ImageEditorDialog';
import { formatOpenDateRange } from '@/ui/format';
import { useMutation, useQuery } from 'urql';
import { useActions } from '@/lib/actions';
import { coupleActions } from '@/lib/actions/couple';
import { ActivityList } from '@/ui/ActivityList';
import { CompetitionHistory } from '@/ui/Competitions';
import { EntityLink } from '@/ui/EntityLink';
import { FederationRankings } from '@/ui/FederationRankings';
import { formatAgeGroup } from '@/ui/csts';
import { FederationCategoryProgress } from '@/ui/FederationCategoryProgress';
import { Tab, TabMenu } from '@/ui/TabMenu';
import { parseAsString, useQueryState } from 'nuqs';
import { DotSeparated } from '@/ui/DotSeparated';
import { User } from 'lucide-react';

export function Couple({
  initialCouple,
}: {
  initialCouple: NonNullable<CoupleQuery['couple']>;
}) {
  const { id } = initialCouple;
  const [editingImage, setEditingImage] = React.useState(false);
  const [, updateImage] = useMutation(UpdateCoupleDocument);
  const [{ data }] = useQuery({ query: CoupleDocument, variables: { id } });
  const item = data?.couple ?? initialCouple;
  const actions = useActions(coupleActions, item);
  const [tab, setTab] = useQueryState(
    'tab',
    parseAsString.withOptions({ history: 'push' }),
  );

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
        onEditImage={
          actions.some((action) => action.id === 'couple.edit')
            ? () => setEditingImage(true)
            : undefined
        }
        title={item.name}
        breadcrumbLabel="Pár"
        breadcrumbs={[{ title: 'Páry', href: '/pary' }]}
        actions={actions}
        subtitle={
          <div className="flex flex-col gap-1.5">
            <DotSeparated>
              {formatAgeGroup(item.ageGroup)}
              {item.federationCompetitors.nodes.map((competitor) => (
                <EntityLink
                  key={competitor.id}
                  type="federation-competitor"
                  entity={competitor}
                  className="text-accent-12"
                  fallback={false}
                >
                  {competitor.federation === 'csts'
                    ? 'ČSTS'
                    : competitor.federation.toUpperCase()}
                </EntityLink>
              ))}
              {formatOpenDateRange(item)}
            </DotSeparated>
            <DotSeparated>
              {item.man ? (
                <EntityLink type="person" entity={item.man}>
                  <User className="inline-flex size-4 mr-0.5" />
                  {item.man.name}
                </EntityLink>
              ) : (
                '?'
              )}
              {item.woman ? (
                <EntityLink type="person" entity={item.woman}>
                  <User className="inline-flex size-4 mr-0.5" />
                  {item.woman.name}
                </EntityLink>
              ) : (
                '?'
              )}
            </DotSeparated>
          </div>
        }
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
      </TabMenu>
    </>
  );
}
