<?php
class DisplayPary {
    public static function viewPartnerRequests($forMe, $byMe) {
        echo Helper::get()->partnerRequest()->getAll();
    }
}