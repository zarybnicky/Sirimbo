import { useCoupleListQuery, useFixUnpairedCouplesMutation } from 'lib/graphql/Couple';
import { Plus } from 'react-feather';
import { useRouter } from 'next/router';
import { List } from 'components/layout/List';
import React from 'react';
import { toast } from 'react-toastify';
import { fromSlugArray } from 'lib/slugify';

export function CoupleList() {
  const router = useRouter();
  const { data, refetch } = useCoupleListQuery();
  const { mutateAsync: doFix } = useFixUnpairedCouplesMutation({
    onSuccess: () => refetch(),
  });

  const id = fromSlugArray(router.query.id);

  const fix = React.useCallback(async () => {
    const data = await doFix({});
    toast.info(`Opraveno ${data.fixUnpairedCouples?.paries?.length || 0} záznamů`);
  }, [doFix]);

  return (
    <List>
      <List.TitleBar title="Páry">
        <List.TitleButton
          active={router.asPath.endsWith('add')}
          icon={Plus}
          href="/admin/pary/add"
        >
          Nový pár
        </List.TitleButton>

        <div className="mt-2 w-full flex gap-2 justify-end">
          <List.TitleButton onClick={fix}>Opravit nespárované páry</List.TitleButton>
        </div>
      </List.TitleBar>

      <List.Scroll>
        {data?.activeCouples?.nodes?.map((item) => (
          <List.Item
            key={item.id}
            active={id === item.id}
            href={{pathname: '/admin/pary/[id]', query: {id: item.id}}}
            title={`${item.userByPIdPartner?.uJmeno} ${item.userByPIdPartner?.uPrijmeni} - ${item.userByPIdPartnerka?.uJmeno} ${item.userByPIdPartnerka?.uPrijmeni}`}
          />
        ))}
      </List.Scroll>
    </List>
  );
}
