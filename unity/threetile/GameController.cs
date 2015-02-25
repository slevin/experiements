using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Slot : Object
{
    public Slot left;
    public Slot right;
    public Slot up;
    public Slot down;

    // where am I relative to the rest of the board

    // who are my neighborss

    public void ClearNieghbors() {
        left = null;
        right = null;
        up = null;
        right = null;
    }
}

public class Board : Object
{
    private HashSet<Slot> slots;
    private int columns;
    private int rows;

    public Board(int columns, int rows) {
        this.slots = new HashSet<Slot>();
        this.columns = columns;
        this.rows = rows;

        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < columns; x++) {
                
            }
        }
    }

    ~Board() {
        // remove circular references
        foreach (Slot s in this.slots) {
            s.ClearNieghbors();
        }
        this.slots.Clear();
    }
}

public class GameController : MonoBehaviour
{
    public GameObject tileQuad;

    // Use this for initialization
    void Start()
    {

        HashSet<Slot> board = new HashSet<Slot>();


        // my physical location (slot should know)



        GameObject topLeft = Instantiate (tileQuad, new Vector3 (-1.1f, 1.1f), Quaternion.identity) as GameObject;
        topLeft.renderer.material.mainTextureOffset = new Vector2 (0f, 0.66f);
        GameObject topMiddle = Instantiate (tileQuad, new Vector3 (0f, 1.1f), Quaternion.identity) as GameObject;
        topMiddle.renderer.material.mainTextureOffset = new Vector2 (0.33f, 0.66f);
        GameObject topRight = Instantiate (tileQuad, new Vector3 (1.1f, 1.1f), Quaternion.identity) as GameObject;
        topRight.renderer.material.mainTextureOffset = new Vector2 (0.66f, 0.66f);

        GameObject left = Instantiate (tileQuad, new Vector3 (-1.1f, 0f), Quaternion.identity) as GameObject;
        left.renderer.material.mainTextureOffset = new Vector2 (0f, 0.33f);


    }

    
    // Update is called once per frame
    void Update()
    {
    
    }
}
