import { useCoupleQuery, useDeleteCoupleMutation } from 'lib/graphql/Couple';
import { useRouter } from 'next/router';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { Item } from 'components/layout/Item';
import { DeleteButton } from 'components/DeleteButton';
import { CoupleList } from 'components/CoupleList';
import { shortDateFormatter } from 'lib/format-date';
import { Card } from 'components/Card';
import { fromSlugArray } from 'lib/slugify';
import { type NextPageWithLayout } from 'pages/_app';

const Page: NextPageWithLayout = () => {
  const router = useRouter();
  const id = fromSlugArray(router.query.id);
  const { data } = useCoupleQuery({ id }, { enabled: !!id, cacheTime: 0 });
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
        <DeleteButton onDelete={() => doDelete({ id })} title="smazat pár" />
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
};

Page.list = <CoupleList />;
Page.isDetail = true;
Page.permissions = [PermissionKey.pePary, PermissionLevel.P_OWNED];

export default Page;
