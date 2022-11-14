import * as React from 'react';
import { AnnouncementList } from '../components/AnnouncementList';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { MyLessonsList } from 'components/MyLessonsList';

export default function DashboardPage() {
  useRequireUserLoggedIn();

  return <div className="container mx-auto max-w-5xl pt-12 pb-8">
    <div className="grid lg:grid-cols-2">
      <div>
        <h4 className="text-right">Moje tréninky</h4>
        <MyLessonsList />
      </div>
      <div>
        <h4 className="text-right">Nástěnka</h4>
        <AnnouncementList />
      </div>
    </div>
  </div>;
}
