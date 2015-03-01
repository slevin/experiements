using UnityEngine;
using System.Collections;

public class TileScript : MonoBehaviour {

    public Tile tile;

    void OnMouseDown() {
        tile.OnClick();
    }

    void Update() {
        if (tile and tile.tileMove) {
            float sqrRemainingDistance = (transform.position - tile.tileMove.TargetPosition()).sqrMagnitude;
            bool shouldMove = sqrRemainingDistance > float.Epsilon;
            if (shouldMove) {
                float moveAmount = tile.tileMove.distanceOverTime * Time.deltaTime;
                Vector3 newPosition = Vector3.MoveTowards(transform.position,
                                                          tile.TargetPosition(),
                                                          moveAmount);
            } else {
                tile.tileMove.CompleteMove();
            }

        }
    }
}
