import { Pencil } from 'lucide-react';
import { defineActions } from '@/lib/actions';
import { EventSeriesForm } from '@/ui/forms/EventSeriesForm';

type EventSeriesActionItem = {
  id: string;
  name?: string | null;
};

export const eventSeriesActions = defineActions<EventSeriesActionItem>()([
  {
    id: 'eventSeries.edit',
    label: 'Upravit sérii',
    icon: Pencil,
    group: 'primary',
    requireTrainer: true,
    render: ({ item }) => <EventSeriesForm series={item} />,
  },
]);
