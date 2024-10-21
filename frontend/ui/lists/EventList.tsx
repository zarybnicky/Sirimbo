import { EventListDocument } from '@/graphql/Event';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { TextField } from '@/ui/fields/text';
import { fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { useAuth } from '@/ui/use-auth';
import { useFuzzySearch } from '@/ui/use-fuzzy-search';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { add, endOf, startOf } from 'date-arithmetic';
import React from 'react';
import { Virtuoso } from 'react-virtuoso';
import { useQuery } from 'urql';
import { z } from 'zod';
import { UpsertEventForm } from '../event-form/UpsertEventForm';
import { ListItem } from '../ListItem';

const QueryParams = z.object({
  id: zRouterId,
});

export function EventList() {
  const [cursor, setCursor] = React.useState<number | undefined>(undefined);
  const [{ data, fetching }] = useQuery({
    query: EventListDocument,
    variables: { first: 100, cursor },
  });
  const loadMore = React.useCallback(() => {
    const info = data?.events?.pageInfo;
    if (info?.endCursor) {
      setCursor(info.endCursor);
    }
  }, [data]);
  const hasMore = data?.events?.pageInfo.hasNextPage !== false;

  const nodes = React.useMemo(() => {
    return (data?.events?.nodes || []).map((x) => {
      let closestInstance = x.eventInstancesList[0];
      const refDate = new Date().getTime();
      for (const instance of x.eventInstancesList) {
        const intervalA = new Date(closestInstance!.since).getTime() - refDate;
        const intervalB = new Date(instance.since).getTime() - refDate;
        if ((intervalA < 0 && intervalB > intervalA)) {
          closestInstance = instance;
        }
      }

      return {
        id: x.id,
        title: x.name,
        date: closestInstance?.since || '',
        subtitle: [
          closestInstance ?
            fullDateFormatter.formatRange(new Date(closestInstance.since), new Date(closestInstance.until)) : '',
          x.location?.name,
          x.locationText,
          (x.capacity ?? 0) > 0 ? `Zbývá ${x.remainingPersonSpots} míst z ${x.capacity}` : '',
        ].filter(Boolean).join(', '),
        href: `/akce/${x.id}`,
      };
    }).sort((a, b) => b.date?.localeCompare(a.date));
  }, [data]);
  const router = useTypedRouter(QueryParams);
  const { id: currentId } = router.query;
  const [search, setSearch] = React.useState('');
  const fuzzy = useFuzzySearch(nodes, ['id', 'title'], search);
  const auth = useAuth();

  const emptyEvent = React.useMemo(() => {
    const day = startOf(endOf(new Date(), 'week', 1), 'day');
    return {
      start: add(day, 9, 'hours'),
      end: add(day, 17, 'hours'),
      action: 'click' as const,
      slots: [],
    };
  }, []);

  return (
    <div className="flex flex-col h-full">
      <div className="px-1 py-4 flex items-center justify-between flex-wrap">
        <div className="font-bold first-letter:uppercase">Akce</div>

        {auth.isTrainerOrAdmin && (
          <Dialog modal={false}>
            <DialogTrigger.Add size="sm" text="Přidat událost" />
            <DialogContent className="sm:max-w-xl">
              <UpsertEventForm slot={emptyEvent} />
            </DialogContent>
          </Dialog>
        )}

        <TextField
          type="search"
          className="w-full mt-2"
          placeholder="Vyhledat..."
          value={search}
          onChange={(e) => setSearch(e.currentTarget.value)}
        />
      </div>

      <Virtuoso
        className="grow h-full overflow-y-auto scrollbar"
        data={fuzzy}
        itemContent={ListItem}
        components={{ Footer: hasMore ? Footer : undefined }}
        context={{ currentId, loading: fetching, loadMore }}
      />
    </div>
  );
}

type FooterContext = { loadMore: () => void; loading: boolean };
const Footer = ({ context }: { context?: FooterContext }) => {
  return (
    <div className="p-2 flex justify-center">
      <SubmitButton type="button" disabled={context?.loading} onClick={context?.loadMore}>
        Načíst starší...
      </SubmitButton>
    </div>
  );
};
