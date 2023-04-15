import { useCoupleQuery, useDeleteCoupleMutation } from 'lib/graphql/Couple';
import { useRouter } from 'next/router';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { Layout } from 'components/layout/Layout';
import { CoupleList } from 'components/CoupleList';
import { shortDateFormatter } from 'lib/format-date';
import { Card } from 'components/Card';

export default function CoupleEditPage() {
  const router = useRouter();
  const { id } = router.query;
  const { data } = useCoupleQuery({ id: id as string }, { enabled: !!id, cacheTime: 0 });
  const { mutateAsync: doDelete } = useDeleteCoupleMutation({
    onSuccess: () => router.push('/admin/nastenka'),
  });
  const item = data?.pary;
  return (
    <Item>
      <Item.Titlebar
        backHref="/admin/pary"
        title={`${item?.userByPIdPartner?.uJmeno} ${item?.userByPIdPartner?.uPrijmeni} - ${item?.userByPIdPartnerka?.uJmeno} ${item?.userByPIdPartnerka?.uPrijmeni}`}
      >
        <DeleteButton
          onDelete={() => doDelete({ id: id as string })}
          title="smazat pár"
        />
      </Item.Titlebar>
      Lekce
      {data?.pary?.rozpisItemsByRiPartner?.nodes.map((item) => (
        <Card key={item.id}>
          {item.rozpiByRiIdRodic?.rDatum &&
            shortDateFormatter.format(new Date(item.rozpiByRiIdRodic?.rDatum))}{' '}
          {item?.rozpiByRiIdRodic?.userByRTrener?.fullName}
        </Card>
      ))}
    </Item>
  );
}

CoupleEditPage.getLayout = (page: React.ReactElement) => (
  <Layout list={<CoupleList />} isDetail>
    {page}
  </Layout>
);

export const getServerSideProps = withServerPermissions(
  PermissionKey.pePary,
  PermissionLevel.P_OWNED,
);
