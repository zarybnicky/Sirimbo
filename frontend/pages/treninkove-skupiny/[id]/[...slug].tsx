import { Layout } from '@/components/layout/Layout';
import {
  CohortDocument,
  type CohortFragment,
  CohortWithMembersDocument,
  type CohortWithMembersQuery,
} from '@/graphql/Cohorts';
import { fetchGql } from '@/graphql/query';
import { CohortList } from '@/ui/lists/CohortList';
import { RichTextView } from '@/ui/RichTextView';
import { TitleBar } from '@/ui/TitleBar';
import { WithSidebar } from '@/ui/WithSidebar';
import { CohortForm } from '@/ui/forms/CohortForm';
import { slugify } from '@/ui/slugify';
import { typographyCls } from '@/ui/style';
import { useAuth } from '@/ui/use-auth';
import { zRouterString } from '@/ui/useTypedRouter';
import type { GetStaticProps } from 'next';
import React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { buttonCls } from '@/ui/style';
import { exportCohort } from '@/ui/reports/export-cohort';
import { DropdownMenuTrigger } from '@/ui/dropdown';
import { CohortMembershipMenu } from '@/ui/menus/CohortMembershipMenu';
import Link from 'next/link';
import { formatLongCoupleName, formatOpenDateRange } from '@/ui/format';
import { TabMenu } from '@/ui/TabMenu';
import { useTenant } from '@/ui/useTenant';
import type { CoupleFragment } from '@/graphql/Memberships';

const QueryParams = z.object({
  id: zRouterString,
  slug: zRouterString,
});

type PageProps = {
  item: CohortFragment;
};

type CohortMembership = NonNullable<
  NonNullable<CohortWithMembersQuery['entity']>['cohortMembershipsList']
>[number];

type CoupleWithMemberships = {
  couple: CoupleFragment;
  manMembership: CohortMembership;
  womanMembership: CohortMembership;
};

type CouplesAndSolos = {
  couples: CoupleWithMemberships[];
  solos: CohortMembership[];
};

function TrainingCohortPage({ item }: PageProps) {
  const auth = useAuth();
  const { data: tenant } = useTenant();
  const { id } = item;
  const [{ data }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id },
    pause: !id,
  });
  const members = React.useMemo(
    () => data?.entity?.cohortMembershipsList ?? [],
    [data?.entity?.cohortMembershipsList],
  );
  const description = React.useMemo(
    () => data?.entity?.description?.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', ''),
    [data?.entity?.description],
  );
  const [tab, setTab] = React.useState<string>('overview');

  const { couples, solos } = React.useMemo<CouplesAndSolos>(
    () => getCouplesAndSolos(members, tenant?.couplesList ?? []),
    [members, tenant?.couplesList],
  );

  const tabs = React.useMemo(
    () => [
      {
        id: 'overview',
        title: 'Přehled',
        contents: () => (
          <OverviewTabContent
            location={data?.entity?.location}
            description={description}
            members={members}
            canManageMemberships={auth.isAdmin}
          />
        ),
      },
      {
        id: 'pairs',
        title: 'Páry a sólisté',
        contents: () => (
          <CouplesAndSolosTabContent
            couples={couples}
            solos={solos}
            canManageMemberships={auth.isAdmin}
          />
        ),
      },
    ],
    [auth.isAdmin, couples, data?.entity?.location, description, members, solos],
  );

  return (
    <Layout hideTopMenuIfLoggedIn>
      <WithSidebar sidebar={<CohortList />}>
        <TitleBar title={data?.entity?.name}>
          {auth.isTrainerOrAdmin && (
            <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={() => exportCohort([id], data?.entity?.name)}>
              Export členů
            </button>
          )}
          {auth.isAdmin && (
            <Dialog>
              <DialogTrigger.Edit size="sm" />
              <DialogContent>
                <CohortForm id={id} />
              </DialogContent>
            </Dialog>
          )}
        </TitleBar>
        <div className="mt-6">
          <TabMenu selected={tab} onSelect={setTab} options={tabs} />
        </div>
      </WithSidebar>
    </Layout>
  );
};

export default TrainingCohortPage;

export const getStaticPaths = () => ({ paths: [], fallback: 'blocking' });
export const getStaticProps: GetStaticProps<PageProps> = async (context) => {
  const params = QueryParams.parse(context.params);
  let { id } = params
  if (!id) {
    id = params.slug;
  }
  const item = await fetchGql(CohortDocument, { id }).then((x) => x.entity);

  if (!item) {
    return {
      revalidate: 60,
      notFound: true,
    };
  }

  const expectedSlug = slugify(item.name);
  if (params.slug !== expectedSlug) {
    return {
      revalidate: 60,
      redirect: {
        destination: `/treninkove-skupiny/${item.id}/${expectedSlug}`,
        permanent: false,
      },
    };
  }

  return {
    revalidate: 60,
    props: { item },
  };
};

function getCouplesAndSolos(
  members: CohortMembership[],
  tenantCouples: (CoupleFragment | null | undefined)[],
): CouplesAndSolos {
  const membershipByPerson = new Map<string, CohortMembership>();
  for (const membership of members) {
    const personId = membership.person?.id;
    if (personId) {
      membershipByPerson.set(personId, membership);
    }
  }

  const usedPersonIds = new Set<string>();
  const couplesWithMemberships: CoupleWithMemberships[] = [];

  for (const couple of tenantCouples) {
    if (!couple || !couple.active) continue;
    const manId = couple.man?.id;
    const womanId = couple.woman?.id;
    if (!manId || !womanId) continue;

    const manMembership = membershipByPerson.get(manId);
    const womanMembership = membershipByPerson.get(womanId);

    if (manMembership && womanMembership) {
      usedPersonIds.add(manId);
      usedPersonIds.add(womanId);
      couplesWithMemberships.push({ couple, manMembership, womanMembership });
    }
  }

  couplesWithMemberships.sort((a, b) =>
    formatLongCoupleName(a.couple).localeCompare(formatLongCoupleName(b.couple), 'cs'),
  );

  const soloMembers = members
      .filter((x) => x.person?.id && !usedPersonIds.has(x.person?.id))
      .sort((a, b) => (a.person?.name || '').localeCompare(b.person?.name || '', 'cs'));

  return { couples: couplesWithMemberships, solos: soloMembers };
}

type MemberRowProps = {
  membership: CohortMembership;
  canManageMemberships: boolean;
};

function FullMemberRow({ membership, canManageMemberships }: MemberRowProps) {
  return (
    <div className="flex gap-3 mb-1 align-baseline">
      {canManageMemberships && (
        <CohortMembershipMenu data={membership}>
          <DropdownMenuTrigger.RowDots />
        </CohortMembershipMenu>
      )}
      <div className="grow gap-2 align-baseline flex flex-wrap justify-between text-sm py-1">
        {membership.person ? (
          <Link
            className="underline font-bold"
            href={{
              pathname: '/clenove/[id]',
              query: { id: membership.person?.id },
            }}
          >
            {membership.person?.name}
          </Link>
        ) : (
          '?'
        )}
        <span>{formatOpenDateRange(membership)}</span>
      </div>
    </div>
  );
}

type OverviewTabContentProps = {
  location?: string | null;
  description?: string | null;
  members: CohortMembership[];
  canManageMemberships: boolean;
};

function OverviewTabContent({
  location,
  description,
  members,
  canManageMemberships,
}: OverviewTabContentProps) {
  return (
    <>
      <h6 className="font-bold mb-2">{location}</h6>
      <RichTextView value={description} />

      {members.length > 0 && (
        <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
          Členové ({members.length})
        </h3>
      )}
      {members.map((membership) => (
        <FullMemberRow
          key={membership.id}
          membership={membership}
          canManageMemberships={canManageMemberships}
        />
      ))}
    </>
  );
}

type CouplesAndSolosTabContentProps = {
  couples: CoupleWithMemberships[];
  solos: CohortMembership[];
  canManageMemberships: boolean;
};

function CouplesAndSolosTabContent({
  couples,
  solos,
  canManageMemberships,
}: CouplesAndSolosTabContentProps) {
  const renderQuickMember = (membership: CohortMembership) => (
    <div className="flex items-center gap-2 text-sm">
      {canManageMemberships && (
        <CohortMembershipMenu data={membership}>
          <DropdownMenuTrigger.RowDots />
        </CohortMembershipMenu>
      )}
      {membership.person ? (
        <Link
          className="underline"
          href={{
            pathname: '/clenove/[id]',
            query: { id: membership.person?.id },
          }}
        >
          {membership.person?.name}
        </Link>
      ) : (
        <span>?</span>
      )}
    </div>
  );

  return (
    <div className="space-y-6">
      <section>
        <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
          Páry ({couples.length})
        </h3>
        {couples.length === 0 ? (
          <p className="text-sm text-neutral-11">Žádné páry v této skupině zatím nejsou.</p>
        ) : (
          <ul className="grid gap-3 text-sm text-neutral-12">
            {couples.map(({ couple, manMembership, womanMembership }) => (
              <li key={couple.id} className="grid grid-cols-2 items-center gap-3">
                {renderQuickMember(manMembership)}
                {renderQuickMember(womanMembership)}
              </li>
            ))}
          </ul>
        )}
      </section>

      <section>
        <h3 className={typographyCls({ variant: 'section', className: 'my-3' })}>
          Sólisté ({solos.length})
        </h3>
        {solos.length === 0 ? (
          <p className="text-sm text-neutral-11">Žádní sólisté.</p>
        ) : (
          <ul className="space-y-2 text-sm text-neutral-12">
            {solos.map((membership) => (
              <li key={membership.id}>{renderQuickMember(membership)}</li>
            ))}
          </ul>
        )}
      </section>
    </div>
  );
}
