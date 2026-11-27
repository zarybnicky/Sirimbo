import { Pencil } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { EventSeriesForm } from '@/ui/forms/EventSeriesForm';

type EventSeriesActionItem = {
  id: string;
  name?: string | null;
  eventsList: { managerPersonIds: string[] }[];
};

export const eventSeriesActions = defineActions<EventSeriesActionItem>()([
  {
    id: 'eventSeries.edit',
    label: 'Upravit sérii',
    icon: Pencil,
    group: 'primary',
    visible: ({ auth, item }) =>
      auth.isAdmin || item.eventsList.some((x) => auth.canManageEvent(x)),
    render: ({ item }) => <EventSeriesForm series={item} />,
  },
]);
