<?php
class DBAkce extends Database implements Pagable {
    public static function getAkce($onlyVisible = false) {
        $res = DBAkce::query('SELECT * FROM akce' . 
                ($onlyVisible ? " WHERE a_visible='1'" : '') . ' ORDER BY a_od');
        return DBAkce::getArray($res);
    }
    
    public static function getPage($offset, $count, $options = '') {
        list($offset, $count, $options) =
            DBAkce::escapeArray(array($offset, $count, $options));
        
        $res = DBAkce::query("SELECT * FROM akce " .
            ($options ? $options : '') . " LIMIT $offset,$count");
        return DBAkce::getArray($res);
    }
    public static function getCount($options = null) {
        $res = DBRozpis::query("SELECT COUNT(*) FROM akce");
        if(!$res) {
            return false;
        } else {
            $row = DBRozpis::getSingleRow($res);
            return $row['COUNT(*)'];
        }
    }
    
    public static function getSingleAkce($id, $onlyVisible = false) {
        list($id) = DBAkce::escapeArray(array($id));
        
        $res = DBAkce::query("SELECT * FROM akce WHERE a_id='$id'" . 
                ($onlyVisible ? " AND a_visible='1'" : '') . ' ORDER BY a_od');
        if(!$res) {
            return false;
        } else {
            return DBAkce::getSingleRow($res);
        }
    }
    
    public static function getAkceItems($id) {
        list($id) = DBAkce::escapeArray(array($id));
        
        $res = DBAkce::query(
        "SELECT *
        FROM akce_item
        LEFT JOIN users ON ai_user=u_id
        WHERE ai_id_rodic='$id' ORDER BY u_prijmeni");
        return DBAkce::getArray($res);
    }
    
    public static function addAkce($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) {
        list($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) =
            DBRozpis::escapeArray(array($jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible));
        
        DBRozpis::query("INSERT INTO akce (a_jmeno,a_kde,a_info,a_od,a_do,a_kapacita,a_dokumenty,a_lock,a_visible)" .
            " VALUES ('$jmeno','$kde','$info','$od','$do','$kapacita','$dokumenty','$lock','$visible')");
        
        return self::getInsertId();
    }
    
    public static function editAkce($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) {
        list($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible) =
            DBRozpis::escapeArray(array($id, $jmeno, $kde, $info, $od, $do, $kapacita, $dokumenty, $lock, $visible));
        
        DBRozpis::query("UPDATE akce SET a_jmeno='$jmeno',a_kde='$kde',a_info='$info',a_od='$od',a_do='$do'," .
            "a_kapacita='$kapacita',a_dokumenty='$dokumenty',a_lock='$lock',a_visible='$visible' WHERE a_id='$id'");
        
        return true;
    }
    
    public static function removeAkce($id) {
        list($id) = DBRozpis::escapeArray(array($id));
        
        DBRozpis::query("DELETE FROM akce WHERE a_id='$id'");
        DBRozpis::query("DELETE FROM akce_item WHERE ai_id_rodic='$id'");
        
        return true;
    }
    
    public static function signUp($user_id, $parent_id, $rok_narozeni) {
        list($user_id, $parent_id, $rok_narozeni) =
            DBAkce::escapeArray(array($user_id, $parent_id, $rok_narozeni));
        
        DBAkce::query("INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)" .
            " VALUES ('$parent_id','$user_id','$rok_narozeni')");
        
        return true;
    }
    
    public static function signOut($user_id, $parena_id) {
        list($user_id, $parena_id) = DBAkce::escapeArray(array($user_id, $parena_id));
        
        DBAkce::query("DELETE FROM akce_item WHERE ai_user='$user_id' AND ai_id_rodic='$parena_id'");
        
        return true;
    }
    
    public static function getAkceName($id) {
        list($id) = DBRozpis::escapeArray(array($id));
        
        $res = DBRozpis::query("SELECT a_jmeno FROM akce WHERE a_id='$id'");
        if(!$res) {
            return false;
        } else {
            $row = DBRozpis::getSingleRow($res);
            
            return $row["a_jmeno"];
        }
    }
    
    public static function addAkceItem($p_id, $u_id, $rok) {
        list($p_id, $u_id, $rok) =
            DBRozpis::escapeArray(array($p_id, $u_id, $rok));
        
        DBRozpis::query("INSERT INTO akce_item (ai_id_rodic,ai_user,ai_rok_narozeni)" .
            " VALUES ('$p_id','$u_id','$rok')");
        
        return true;
    }
    
    public static function editAkceItem($id, $u_id, $rok) {
        list($id, $u_id, $rok) =
            DBRozpis::escapeArray(array($id, $u_id, $rok));
    
        DBRozpis::query("UPDATE akce_item SET ai_user='$u_id',ai_rok_narozeni='$rok' WHERE" .
            " ai_id='$id'");
                
        return true;
    }
    
    public static function removeAkceItem($id) {
        list($id) = DBRozpis::escapeArray(array($id));
        
        DBRozpis::query("DELETE FROM akce_item WHERE ai_id='$id'");
        
        return true;
    }
    
    public static function isUserSignedUp($a_id, $u_id) {
        list($a_id, $u_id) = DBRozpis::escapeArray(array($a_id, $u_id));
        
        $res = DBRozpis::query("SELECT ai_id FROM akce_item WHERE ai_id_rodic='$a_id' AND ai_user='$u_id'");
        if(!$res) {
            return false;
        } else {
            $row = DBRozpis::getSingleRow($res);
            return (bool)$row["ai_id"];
        }
    }
}
?>